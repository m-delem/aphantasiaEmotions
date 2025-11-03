# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, glue, ggplot2, ggpubr, 
  mgcv, marginaleffects, modelbased, 
  patchwork, scales
)
source("R/ggplot_utils.R")

gam_style <- function(
    h_caption = 0, 
    plot_margin = margin(3.5, 8, 3.5, 3.5),
    ...
) {
  style <-
    list(
      scale_x_continuous(
        breaks = seq(16, 80, by = 4),
        expand = expansion(mult = c(0.02, 0.02))
      ),
      scale_y_continuous(breaks = breaks_pretty(10)),
      theme_pdf(
        base_theme = theme_minimal,
        plot.caption = element_text(
          hjust = h_caption,
          margin = margin(t = 8), 
          size = rel(0.9),
          color = "grey40"
        ),
        plot.margin = plot_margin,
        panel.border = ggplot2::element_rect(color = "grey50", fill = NA),
        ...
      )
    )
}

plot_gam_means <- function(
    model, 
    length = 65,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_title = "VVIQ score",
    y_title = "Score"
) {
  means <- estimate_means(model, by = "vviq", length = length)
  
  p <- 
    plot(means) +
    labs(
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
    length = 65,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_title = "VVIQ score",
    y_title = "Score"
) {
  relations <- estimate_relation(model, length = length)
  
  p <- 
    plot(relations) +
    labs(
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
    length = 65,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x_title = "VVIQ score",
    y_title = "Slope (score variation per unit change in VVIQ)"
) {
  slopes <- 
    estimate_slopes(
      model, 
      trend = "vviq", 
      by = "vviq", 
      length = length
    )
  
  p <- 
    plot(slopes) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      caption = caption
    ) +
    scale_discrete_manual(
      aesthetics = c("color", "fill"),
      name = "p-value",
      values = palette.colors()[c(1, 4)],
      labels = c(
        "not significant" = "Not significant",
        "significant"     = "Significant"
      )
    ) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  return(p)
}