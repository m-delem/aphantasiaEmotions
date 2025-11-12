superb_with_params <- function(..., dodge_width = 0.4) {
  rlang::check_installed("superb", reason = "to use `superb_with_params()`.")
  
  p <- 
    superb::superb(
      ...,
      plotLayout = "raincloud",
      pointParams = list(
        size  = 1.5,
        alpha = 1,
        position = ggplot2::position_dodge(width = dodge_width),
        color = palette.colors()[1:4]
      ),
      jitterParams = list(
        size = 0.75,
        alpha = 0.1,
        width = 0.1
      ),
      errorbarParams = list(
        linewidth = 0.3,
        width = 0,
        position = ggplot2::position_dodge(width = dodge_width),
        color = palette.colors()[1:4]
      ),
      violinParams = list(
        trim  = TRUE,
        width = 0.7,
        linewidth = 0.1
      ),
    )
  
  return(p)
}

customise_superb_plot <- function(p, title = NULL, y_label = "Score", ...) {
  custom_p <- 
    p +
    scale_x_aphantasia() +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(10),
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = 0.7,
      axis_relative_x = 1,
      axis_relative_y = 0.9,
      legend_relative = 1,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.caption = ggplot2::element_text(
        margin = ggplot2::margin(t = 8),
        size = ggplot2::rel(1),
        color = "grey40"
      ),
      panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
      ...
    )
  
  return(custom_p)
}

fix_superb_aes <- function(p) {
  q <- ggplot2::ggplot_build(p)
  q$plot$mapping <- 
    ggplot2::aes(
      x = vviq_group_4, 
      y = center, 
      colour = vviq_group_4, 
      fill = vviq_group_4,
      shape = NULL
    )
  p <- q$plot
  return(p)
}