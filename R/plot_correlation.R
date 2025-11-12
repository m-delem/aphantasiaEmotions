plot_correlation <- function(
    y, x = "vviq",
    grouping = "vviq_group_4",
    df = tas_data,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_label = "VVIQ Score",
    y_label = "Score",
    coef_coords = c(30, 25),
    nl_model = FALSE,
    ...
) {
  rlang::check_installed("ggpubr", reason = "to use `plot_correlation()`.")
  
  p <- 
    df |> 
    ggpubr::ggscatter(
      x = x,
      y = y,
      color = grouping,
      size  = 0.6,
      mean.point      = TRUE,
      mean.point.size = 2,
      star.plot       = TRUE,
      star.plot.lwd   = .01,
      cor.coef = TRUE,
      cor.method = "spearman",
      cor.coef.coord = coef_coords
    ) +
    ggplot2::geom_smooth(
      method    = "lm",
      formula   = y ~ x,
      color     = "black",
      linewidth = .3,
      alpha     = .2,
      fullrange = TRUE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(16, 80, by = 4),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(10),
    ) +
    theme_pdf(
      ggplot2::theme_minimal,
      plot.caption = ggplot2::element_text(
        margin = ggplot2::margin(t = 8), 
        size = ggplot2::rel(1),
        color = "grey40"
      ),
      panel.border = ggplot2::element_rect(color = "grey50", fill = NA),
    )
  
  if (nl_model) {
    p <- 
      p +
      ggplot2::geom_smooth(
        method    = "loess",
        formula   = y ~ x,
        color     = "black",
        linetype  = "dashed",
        linewidth = .3,
        alpha     = .2,
        fullrange = TRUE,
        span = 0.7,
        se = FALSE
      )
  }
  
  return(p)
}

