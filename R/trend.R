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

# Poids moyen des 7 derniers jours vs des 7 jours précédents
week_over_week <- function(df) {
  df <- df[order(df$date), ]
  fin <- max(df$date)
  semaine_recente <- df[df$date > fin - 7, ]
  semaine_precedente <- df[df$date <= fin - 7 & df$date > fin - 14, ]

  if (nrow(semaine_recente) == 0 || nrow(semaine_precedente) == 0) {
    return(list(moyenne_recente = NA_real_, moyenne_precedente = NA_real_, delta = NA_real_))
  }

  moyenne_recente <- mean(semaine_recente$poids)
  moyenne_precedente <- mean(semaine_precedente$poids)
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
  wow <- week_over_week(df)
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
