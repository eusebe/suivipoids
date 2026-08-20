# Moyenne mobile simple, centrée si possible sinon glissante en fin de série
rolling_mean <- function(x, n) {
  vapply(seq_along(x), function(i) {
    lo <- max(1, i - n + 1)
    mean(x[lo:i], na.rm = TRUE)
  }, numeric(1))
}

# Ajoute une colonne poids_lisse (moyenne mobile sur `window` jours)
add_smoothed <- function(df, window = 7) {
  df <- df[order(df$date), ]
  df$poids_lisse <- rolling_mean(df$poids, window)
  df
}

# Restreint aux `n_jours` derniers jours de la série, ou tout si n_jours = Inf
window_data <- function(df, n_jours = Inf) {
  df <- df[order(df$date), ]
  if (is.infinite(n_jours)) return(df)
  seuil <- max(df$date) - n_jours
  df[df$date > seuil, ]
}

# Régression linéaire poids ~ date sur la fenêtre choisie
# Retourne la pente en kg/semaine, l'ordonnée et le modèle
fit_trend <- function(df, n_jours = Inf) {
  d <- window_data(df, n_jours)
  if (nrow(d) < 2) {
    return(list(pente_semaine = NA_real_, modele = NULL, n = nrow(d)))
  }
  d$date_num <- as.numeric(d$date)
  modele <- lm(poids ~ date_num, data = d)
  pente_jour <- unname(coef(modele)["date_num"])
  list(
    pente_semaine = pente_jour * 7,
    modele = modele,
    n = nrow(d)
  )
}

# Projette la date à laquelle le poids cible sera atteint, en extrapolant
# la régression linéaire ajustée sur la fenêtre choisie.
project_target <- function(df, target, n_jours = Inf) {
  trend <- fit_trend(df, n_jours)
  poids_actuel <- df$poids[which.max(df$date)]
  kg_restant <- poids_actuel - target

  if (is.null(trend$modele) || is.na(trend$pente_semaine) || trend$pente_semaine == 0) {
    return(list(
      pente_semaine = trend$pente_semaine,
      date_estimee = as.Date(NA),
      jours_restants = NA_real_,
      kg_restant = kg_restant,
      atteignable = FALSE
    ))
  }

  pente_jour <- trend$pente_semaine / 7
  # Objectif atteignable seulement si la tendance va dans le bon sens
  atteignable <- sign(pente_jour) == sign(target - poids_actuel) || kg_restant == 0

  coefs <- coef(trend$modele)
  date_num_cible <- (target - coefs["(Intercept)"]) / coefs["date_num"]
  date_estimee <- as.Date(unname(date_num_cible), origin = "1970-01-01")
  jours_restants <- as.numeric(date_estimee - max(df$date))

  list(
    pente_semaine = trend$pente_semaine,
    date_estimee = if (atteignable) date_estimee else as.Date(NA),
    jours_restants = if (atteignable) jours_restants else NA_real_,
    kg_restant = kg_restant,
    atteignable = atteignable
  )
}

# Poids moyen sur les `n_jours` derniers jours vs les `n_jours` précédents
periode_over_periode <- function(df, n_jours = 7) {
  df <- df[order(df$date), ]
  fin <- max(df$date)
  periode_recente <- df[df$date > fin - n_jours, ]
  periode_precedente <- df[df$date <= fin - n_jours & df$date > fin - 2 * n_jours, ]

  if (nrow(periode_recente) == 0 || nrow(periode_precedente) == 0) {
    return(list(moyenne_recente = NA_real_, moyenne_precedente = NA_real_, delta = NA_real_))
  }

  moyenne_recente <- mean(periode_recente$poids)
  moyenne_precedente <- mean(periode_precedente$poids)
  list(
    moyenne_recente = moyenne_recente,
    moyenne_precedente = moyenne_precedente,
    delta = moyenne_recente - moyenne_precedente
  )
}

# Prochain palier intermédiaire (au pas `pas` kg) entre le poids actuel et l'objectif
next_milestone <- function(poids_actuel, target, pas = 1) {
  if (poids_actuel == target) return(target)
  direction <- sign(target - poids_actuel)
  palier <- if (direction < 0) floor(poids_actuel / pas) * pas else ceiling(poids_actuel / pas) * pas
  if (palier == poids_actuel) palier <- palier + direction * pas
  if (direction < 0) max(palier, target) else min(palier, target)
}

# Poids théorique estimé à une date donnée, en extrapolant la régression
weight_at_date <- function(df, date_cible, n_jours = Inf) {
  trend <- fit_trend(df, n_jours)
  if (is.null(trend$modele)) return(NA_real_)
  coefs <- coef(trend$modele)
  unname(coefs["(Intercept)"] + coefs["date_num"] * as.numeric(date_cible))
}

# Nombre de jours distincts pesés sur les `n_jours` derniers jours
regularite_pesees <- function(df, n_jours = 7) {
  d <- window_data(df, n_jours - 1)
  nrow(d)
}

# Plateau : variation quasi nulle sur les 7 derniers jours (vs les 7 précédents)
# alors que la tendance de fond (fenêtre longue) va toujours dans le bon sens
is_plateau <- function(df, target, seuil_kg = 0.3, n_jours_long = 30) {
  wow <- periode_over_periode(df, 7)
  if (is.na(wow$delta)) return(FALSE)
  trend_long <- fit_trend(df, n_jours_long)
  if (is.null(trend_long$modele) || is.na(trend_long$pente_semaine)) return(FALSE)
  poids_actuel <- df$poids[which.max(df$date)]
  tendance_bonne_direction <- sign(trend_long$pente_semaine) == sign(target - poids_actuel)
  abs(wow$delta) < seuil_kg && tendance_bonne_direction
}

# Série (date, poids agrégé/jour) à utiliser pour la tendance/projection.
# Si on distingue les moments et qu'il y a assez de pesées du matin, on se
# base uniquement dessus (plus fiable : le poids du soir est naturellement
# plus élevé et biaiserait la tendance). Sinon, on retombe sur l'agrégat
# journalier toutes pesées confondues.
serie_reference <- function(df_daily, df_brut = NULL, distinguer_moment = FALSE) {
  if (isTRUE(distinguer_moment) && !is.null(df_brut)) {
    matin <- df_brut[df_brut$moment == "Matin", c("date", "poids")]
    if (nrow(matin) >= 2) {
      return(list(serie = agg_daily(matin), base = "matin"))
    }
  }
  list(serie = df_daily, base = "tout")
}

# Historique des paliers (multiples de `pas` kg) franchis depuis la première
# pesée, dans le sens de l'objectif. Un palier est "franchi" à la première
# date où le meilleur poids atteint jusque là (min ou max cumulé selon le
# sens) passe ce seuil. Retourne les paliers les plus récents en premier.
paliers_franchis <- function(df, target, pas = 1) {
  vide <- data.frame(palier = numeric(0), date = as.Date(character(0)))
  df <- df[order(df$date), ]
  if (nrow(df) == 0) return(vide)

  poids_depart <- df$poids[1]
  if (poids_depart == target) return(vide)
  direction <- sign(target - poids_depart)

  depart_arrondi <- if (direction < 0) floor(poids_depart / pas) * pas else ceiling(poids_depart / pas) * pas
  cible_arrondie <- if (direction < 0) ceiling(target / pas) * pas else floor(target / pas) * pas
  if (sign(cible_arrondie - depart_arrondi) != direction) return(vide)

  paliers <- seq(depart_arrondi, cible_arrondie, by = direction * pas)
  paliers <- paliers[sign(paliers - poids_depart) == direction]
  if (length(paliers) == 0) return(vide)

  meilleur <- if (direction < 0) cummin(df$poids) else cummax(df$poids)
  resultats <- lapply(paliers, function(p) {
    idx <- if (direction < 0) which(meilleur <= p)[1] else which(meilleur >= p)[1]
    if (is.na(idx)) return(NULL)
    data.frame(palier = p, date = df$date[idx])
  })
  resultats <- Filter(Negate(is.null), resultats)
  if (length(resultats) == 0) return(vide)
  out <- do.call(rbind, resultats)
  out[order(out$date, decreasing = TRUE), ]
}

# Pente (kg/semaine) calculée sur une fenêtre glissante de `fenetre` jours,
# évaluée tous les `pas_jours` jours : permet de visualiser si le rythme de
# perte/prise accélère ou ralentit dans le temps.
vitesse_glissante <- function(df, fenetre = 14, pas_jours = 7) {
  vide <- data.frame(date = as.Date(character(0)), pente_semaine = numeric(0))
  df <- df[order(df$date), ]
  if (nrow(df) < 2) return(vide)

  dates_eval <- seq(min(df$date) + fenetre, max(df$date), by = pas_jours)
  if (length(dates_eval) == 0 || max(dates_eval) < max(df$date)) {
    dates_eval <- c(dates_eval, max(df$date))
  }

  resultats <- lapply(dates_eval, function(d_fin) {
    sous <- df[df$date > d_fin - fenetre & df$date <= d_fin, ]
    if (nrow(sous) < 2) return(NULL)
    tr <- fit_trend(sous, Inf)
    if (is.null(tr$modele)) return(NULL)
    data.frame(date = d_fin, pente_semaine = tr$pente_semaine)
  })
  resultats <- Filter(Negate(is.null), resultats)
  if (length(resultats) == 0) return(vide)
  do.call(rbind, resultats)
}
