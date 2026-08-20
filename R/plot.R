library(ggplot2)

MOMENT_COULEURS <- c("Matin" = "#2166ac", "Milieu de journée" = "#f39c12", "Soir" = "#762a83")

# Construit le graphique poids. `df` est la série agrégée par jour (une
# ligne/jour) qui pilote le lissage. `proj` est la projection déjà calculée
# par l'appelant (project_target sur la série de référence — cf.
# serie_reference() dans trend.R), pour rester cohérent avec le résumé texte.
#
# Si `distinguer_moment` est vrai et que `df_brut` (une ligne par pesée, avec
# une colonne `moment`) est fourni : points colorés par moment, avec une
# tendance calculée séparément par moment. Sinon : comportement par défaut
# (points uniformes, tendance sélectionnée + tendance depuis le début).
render_weight_plot <- function(df, target, n_jours, proj, df_brut = NULL, distinguer_moment = FALSE,
                                target_intermediaire = NULL, proj_intermediaire = NULL) {
  df <- add_smoothed(df, window = min(7, nrow(df)))
  utiliser_moment <- isTRUE(distinguer_moment) && !is.null(df_brut) && nrow(df_brut) > 0

  p <- ggplot(df, aes(date, poids))

  if (utiliser_moment) {
    p <- p + geom_point(
      data = df_brut, aes(date, poids, color = moment),
      size = 2.4, inherit.aes = FALSE
    )
  } else {
    p <- p + geom_point(size = 2, color = "#2c3e50")
  }

  p <- p + geom_line(aes(y = poids_lisse), color = "#2c3e50", linewidth = 0.8)

  if (utiliser_moment) {
    lignes <- do.call(rbind, lapply(levels(df_brut$moment), function(m) {
      sous <- df_brut[df_brut$moment == m, c("date", "poids")]
      if (nrow(sous) == 0) return(NULL)
      serie <- agg_daily(sous)
      tr <- fit_trend(serie, n_jours)
      if (is.null(tr$modele)) return(NULL)
      d <- window_data(serie, n_jours)
      data.frame(date = d$date, y = predict(tr$modele), moment = m)
    }))
    if (!is.null(lignes)) {
      p <- p + geom_line(
        data = lignes, aes(date, y, color = moment),
        linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE
      )
    }
    p <- p + scale_color_manual(name = NULL, values = MOMENT_COULEURS)
  } else {
    trend_line <- function(n, label) {
      tr <- fit_trend(df, n)
      if (is.null(tr$modele)) return(NULL)
      d <- window_data(df, n)
      data.frame(date = d$date, y = predict(tr$modele), tendance = label)
    }

    lignes <- do.call(rbind, Filter(Negate(is.null), list(
      trend_line(n_jours, "Tendance sélectionnée"),
      if (!is.infinite(n_jours)) trend_line(Inf, "Depuis le début")
    )))

    if (!is.null(lignes)) {
      p <- p +
        geom_line(
          data = lignes, aes(date, y, color = tendance),
          linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE
        ) +
        scale_color_manual(
          name = NULL,
          values = c("Tendance sélectionnée" = "#e67e22", "Depuis le début" = "#95a5a6")
        )
    }
  }

  p <- p + geom_hline(yintercept = target, color = "#27ae60", linetype = "dotted")

  # Si l'objectif est estimé trop loin dans le futur, ne pas étirer l'axe
  # jusque là (ça écraserait les points/tendances récents) : la date reste
  # de toute façon indiquée dans le résumé texte.
  horizon_max <- max(df$date) + 180
  if (isTRUE(proj$atteignable) && !is.na(proj$date_estimee) && proj$date_estimee <= horizon_max) {
    p <- p +
      geom_point(
        data = data.frame(date = proj$date_estimee, poids = target),
        aes(date, poids), color = "#27ae60", size = 3
      ) +
      annotate(
        "text", x = proj$date_estimee, y = target,
        label = "objectif", vjust = -1, color = "#27ae60"
      )
  }

  if (!is.null(target_intermediaire)) {
    p <- p + geom_hline(yintercept = target_intermediaire, color = "#7f8c8d", linetype = "dotted")

    if (!is.null(proj_intermediaire) && isTRUE(proj_intermediaire$atteignable) &&
        !is.na(proj_intermediaire$date_estimee) && proj_intermediaire$date_estimee <= horizon_max) {
      p <- p +
        geom_point(
          data = data.frame(date = proj_intermediaire$date_estimee, poids = target_intermediaire),
          aes(date, poids), color = "#7f8c8d", size = 3
        ) +
        annotate(
          "text", x = proj_intermediaire$date_estimee, y = target_intermediaire,
          label = "intermédiaire", vjust = -1, color = "#7f8c8d"
        )
    }
  }

  p +
    scale_x_date(expand = expansion(mult = c(0.02, 0.12))) +
    labs(x = NULL, y = "Poids (kg)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

# Graphique de l'IMC dans le temps, avec les bandes de catégories OMS en fond
render_bmi_plot <- function(df, target, taille_m = TAILLE_M) {
  d <- df[order(df$date), ]
  d$imc <- imc(d$poids, taille_m)
  d$imc_lisse <- rolling_mean(d$imc, min(7, nrow(d)))

  bornes <- sort(unique(c(
    min(d$imc, d$imc_lisse, IMC_SEUILS[1] - 2, na.rm = TRUE),
    IMC_SEUILS,
    max(d$imc, d$imc_lisse, IMC_SEUILS[length(IMC_SEUILS)] + 2, na.rm = TRUE)
  )))
  bandes <- data.frame(ymin = bornes[-length(bornes)], ymax = bornes[-1])
  bandes$label <- as.character(imc_categorie((bandes$ymin + bandes$ymax) / 2))

  imc_cible <- imc(target, taille_m)

  ggplot() +
    geom_rect(
      data = bandes,
      aes(xmin = min(d$date), xmax = max(d$date), ymin = ymin, ymax = ymax, fill = label),
      alpha = 0.12
    ) +
    scale_fill_manual(name = NULL, values = setNames(IMC_COULEURS, IMC_LABELS)) +
    geom_point(data = d, aes(date, imc), size = 2, color = "#2c3e50") +
    geom_line(data = d, aes(date, imc_lisse), color = "#2c3e50", linewidth = 0.8) +
    geom_hline(yintercept = imc_cible, color = "#27ae60", linetype = "dotted") +
    annotate(
      "text", x = max(d$date), y = imc_cible,
      label = sprintf("IMC cible : %.1f", imc_cible), vjust = -1, hjust = 1, color = "#27ae60"
    ) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = NULL, y = "IMC") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}
