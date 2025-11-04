gam_style <- function(
    h_caption = 0, 
    ...
) {
  style <-
    list(
      ggplot2::scale_x_continuous(
        breaks = seq(16, 80, by = 4),
        expand = ggplot2::expansion(mult = c(0.02, 0.02))
      ),
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(10)),
      theme_pdf(
        base_theme = theme_minimal,
        plot.caption = ggplot2::element_text(
          hjust  = h_caption,
          margin = ggplot2::margin(t = 8), 
          size   = ggplot2::rel(0.9),
          color  = "grey40"
        ),
        panel.border = ggplot2::element_rect(color = "grey50", fill = NA),
        ...
      )
    )
}

plot_gam_means <- function(
    model, 
    by = "vviq",
    length = 65,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    x_title  = "VVIQ score",
    y_title  = "Score"
) {
  rlang::check_installed("modelbased", reason = "to use `plot_gam_means()`.")
  
  means <- modelbased::estimate_means(model, by = by, length = length)
  
  p <- 
    plot(means) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title
    )
  
  return(p)
}

plot_gam_relations <- function(
    model, 
    by = "vviq",
    length = 65,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_title = "VVIQ score",
    y_title = "Score"
) {
  rlang::check_installed(
    "modelbased", 
    reason = "to use `plot_gam_relations()`."
  )
  
  relations <- modelbased::estimate_relation(model, by = by, length = length)
  
  p <- 
    plot(relations) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_title,
      y = y_title
    )
  
  return(p)
}

plot_gam_slopes <- function(
    model,
    trend = "vviq",
    by = "vviq",
    length = 65,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_title = "VVIQ score",
    y_title = "Slope (score variation per unit change in VVIQ)"
) {
  rlang::check_installed("modelbased", reason = "to use `plot_gam_slopes()`.")
  
  slopes <- 
    modelbased::estimate_slopes(
      model, 
      trend = trend, 
      by = by, 
      length = length
    )
  
  p <- 
    plot(slopes) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      caption = caption
    ) +
    ggplot2::scale_discrete_manual(
      aesthetics = c("color", "fill"),
      name = "p-value",
      values = palette.colors()[c(1, 4)],
      labels = c(
        "not significant" = "Not significant",
        "significant"     = "Significant"
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  
  return(p)
}