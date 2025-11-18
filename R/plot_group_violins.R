plot_group_violins <- function(
    formula, 
    data = all_data,
    dot_size = 0.5,
    box.linewidth = 0.1,
    middle.linewidth = 0.5,
    violin_width = 0.7,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_lab = NULL,
    y_lab = "Score",
    breaks = scales::breaks_pretty(10), 
    base_theme = ggplot2::theme_minimal,
    axis_relative_size = 1,
    axis_relative_x = 1,
    axis_relative_y = 0.85,
    ...
) {
  model  <- stats::lm(formula, data)
  p_data <- model$model
  var_1  <- colnames(model$model)[1]
  var_2  <- colnames(model$model)[2]
  
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = p_data,
      ggplot2::aes(
        y = .data[[var_1]],
        x = .data[[var_2]],
        color = .data[[var_2]]
      ),
      size  = dot_size,
      alpha = 0.1,
      position = ggplot2::position_jitter(
        width = 0.125,
        height = 0.25    
      )
    ) +
    ggplot2::geom_crossbar(
      data = modelbased::estimate_means(model, by = var_2),
      ggplot2::aes(
        y = Mean,
        x = .data[[var_2]],
        color = .data[[var_2]],
        fill = .data[[var_2]],
        ymin = CI_low,
        ymax = CI_high
      ),
      alpha = 0.1,
      width = 0.15,
      box.linewidth = box.linewidth,
      middle.linewidth = middle.linewidth
    ) +
    see::geom_violinhalf(
      data = p_data,
      ggplot2::aes(
        y = .data[[var_1]],
        x = .data[[var_2]],
        # color = .data[[var_2]],
        fill = .data[[var_2]]
      ),
      color = "transparent",
      alpha = 0.25,
      linewidth = 0.1,
      width = violin_width,
      position = ggplot2::position_nudge(x = 0.2)
    ) +
    ggplot2::scale_y_continuous(breaks = breaks) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_lab, 
      y = y_lab
    ) +
    theme_pdf(
      base_theme = base_theme,
      axis_relative_size = axis_relative_size,
      axis_relative_x = axis_relative_x,
      axis_relative_y = axis_relative_y,
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      ...
    )
  
  return(p)
}
