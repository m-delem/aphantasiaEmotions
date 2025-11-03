# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, ggpubr, scales)
source("R/ggplot_utils.R")

# Correlation between VVIQ and TAS scores ------------------------------
plot_correlation <- function(
    df,
    grouping,
    n_groups = 2,
    data_name = "",
    title = NULL,
    ...
) {
  p <- 
    df |> 
    ggscatter(
      x = "vviq",
      y = "tas",
      color = grouping,
      size  = 0.6,
      mean.point      = TRUE,
      mean.point.size = 2,
      star.plot       = TRUE,
      star.plot.lwd   = .05,
      cor.coef = TRUE,
      cor.method = "spearman",
      cor.coef.coord = c(30, 25)
    ) +
    geom_hline(
      yintercept = 61,
      linetype = "solid",
      linewidth = 0.3,
      color = "red",
      show.legend = FALSE
    ) +
    annotate(
      "text",
      x = 30,
      y = 63,
      label = "Alexithymia cut-off",
      color = "red",
      hjust = 0,
      size = 2
    ) +
    geom_smooth(
      method    = "lm",
      formula   = y ~ x,
      color     = "black",
      linewidth = .3,
      alpha     = .2,
      fullrange = TRUE
    ) +
    geom_smooth(
      method    = "loess",
      formula   = y ~ x,
      color     = "black",
      linetype  = "dashed",
      linewidth = .3,
      alpha     = .2,
      fullrange = TRUE,
      span = 0.7,
      se = FALSE
    ) +
    labs(
      title = title,
      subtitle = write_sample_info(
        df,
        n_groups = n_groups
      ),
      caption = data_name,
      x = "VVIQ Score",
      y = "TAS Score"
    ) +
    scale_x_continuous(
      breaks = breaks_pretty(20),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      breaks = breaks_pretty(20),
    ) +
    scale_discrete_manual(
      aesthetics = c("color", "fill"),
      name = NULL,
      values = palette.colors(),
      labels = stringr::str_to_title(levels(all_data[[grouping]]))
    ) +
    theme_pdf(
      theme_classic,
      plot.caption = ggplot2::element_text(
        margin = margin(t = 8), 
        size = rel(1),
        color = "grey40"
      ),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
    )
  
  return(p)
}

