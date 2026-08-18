# Génère un jeu de données fictif réaliste pour les tests en local :
# tendance à la baisse, un plateau au milieu, du bruit quotidien, des jours
# sans pesée, et quelques pesées du soir (plus élevées) en plus des pesées
# du matin, pour tester la distinction matin/midi/soir.
generate_fake_weight_data <- function(n_days = 90, poids_depart = 92, pente_semaine = -0.4, seed = 123) {
  set.seed(seed)
  dates <- seq(Sys.Date() - n_days + 1, Sys.Date(), by = "day")
  jours <- seq_len(n_days) - 1

  pente_jour <- pente_semaine / 7
  plateau_debut <- floor(n_days * 0.45)
  plateau_fin <- plateau_debut + 12

  progression <- numeric(n_days)
  cumul <- 0
  for (i in seq_len(n_days)) {
    if (jours[i] < plateau_debut || jours[i] > plateau_fin) {
      cumul <- cumul + pente_jour
    }
    progression[i] <- cumul
  }

  poids_matin <- round(poids_depart + progression + rnorm(n_days, 0, 0.35), 1)
  heure_matin <- as.POSIXct(
    paste(dates, sprintf("%02d:%02d:00", sample(6:9, n_days, TRUE), sample(0:59, n_days, TRUE))),
    tz = "Europe/Paris"
  )

  df <- data.frame(date = dates, heure = heure_matin, poids = poids_matin)

  # Simule des jours sans pesée (garde les 5 derniers jours pour la fraîcheur)
  garder <- (n_days - seq_len(n_days) + 1) <= 5 | runif(n_days) > 0.25
  df <- df[garder, ]

  # Ajoute occasionnellement une pesée du soir (un peu plus élevée)
  a_soir <- runif(nrow(df)) < 0.2
  soirs <- df[a_soir, ]
  if (nrow(soirs) > 0) {
    soirs$heure <- as.POSIXct(
      paste(soirs$date, sprintf("%02d:%02d:00", sample(20:22, nrow(soirs), TRUE), sample(0:59, nrow(soirs), TRUE))),
      tz = "Europe/Paris"
    )
    soirs$poids <- round(soirs$poids + runif(nrow(soirs), 0.8, 1.6), 1)
    df <- rbind(df, soirs)
  }

  df <- df[order(df$heure), ]
  df$moment <- moment_jour(df$heure)
  rownames(df) <- NULL
  df[, c("date", "heure", "moment", "poids")]
}
