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
