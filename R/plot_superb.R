# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, ggpubr, patchwork, scales, superb, tidyr)
source("R/ggplot_utils.R")

superb_with_params <- function(...) {
  dodge_width <- 0.4
  
  p <- 
    superb(
      ...,
      plotLayout = "raincloud",
      pointParams = list(
        size  = 1.5,
        alpha = 1,
        position = position_dodge(width = dodge_width),
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
        position = position_dodge(width = dodge_width),
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

customise_plot <- function(p, title = NULL) {
  custom_p <- 
    p +
    scale_x_discrete(
      labels = c(
        "aphantasia"     = "Aphantasia",
        "hypophantasia"  = "Hypophantasia",
        "typical"        = "Typical",
        "hyperphantasia" = "Hyperphantasia"
      ),
      expand = expansion(
        mult = 0,
        add = c(0, 0.7))
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(10),
    ) +
    scale_discrete_manual(
      aesthetics = c("color", "fill"),
      values = palette.colors()
    ) +
    labs(
      x = NULL,
      y = "Score",
      title = title,
    ) +
    theme_pdf(
      base_theme = theme_minimal,
      axis_relative_size = 0.7,
      axis_relative_x = 1,
      axis_relative_y = 0.9,
      legend_relative = 1,
      # Custom theme arguments
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.caption = ggplot2::element_text(
        margin = margin(t = 8),
        size = rel(1),
        color = "grey40"
      ),
      panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
    )
  
  return(custom_p)
}

fix_aes <- function(p) {
  q <- ggplot_build(p)
  q$plot$mapping <- 
    aes(
      x = vviq_group_4, 
      y = center, 
      colour = vviq_group_4, 
      fill = vviq_group_4,
      shape = NULL
    )
  p <- q$plot
  return(p)
}