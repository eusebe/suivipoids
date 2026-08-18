library(ggplot2)

# Construit le graphique : points, lissage, droite de tendance, objectif et projection
render_weight_plot <- function(df, target, n_jours) {
  df <- add_smoothed(df, window = min(7, nrow(df)))
  trend <- fit_trend(df, n_jours)
  proj <- project_target(df, target, n_jours)

  p <- ggplot(df, aes(date, poids)) +
    geom_point(size = 2, color = "#2c3e50") +
    geom_line(aes(y = poids_lisse), color = "#2c3e50", linewidth = 0.8)

  if (!is.null(trend$modele)) {
    d <- window_data(df, n_jours)
    pred <- data.frame(date = d$date, y = predict(trend$modele))
    p <- p + geom_line(data = pred, aes(date, y), color = "#e67e22", linetype = "dashed")
  }

  p <- p + geom_hline(yintercept = target, color = "#27ae60", linetype = "dotted")

  if (isTRUE(proj$atteignable) && !is.na(proj$date_estimee)) {
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

  p +
    scale_x_date(expand = expansion(mult = c(0.02, 0.12))) +
    labs(x = NULL, y = "Poids (kg)") +
    theme_minimal(base_size = 14)
}
