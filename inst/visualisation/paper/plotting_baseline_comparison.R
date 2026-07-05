# ==============================================================================
# 2-group (naive baseline) + 4-group categorical figure, side by side
# ==============================================================================
library(patchwork)

# ------------------------------------------------------------------------------
# 2-group panel — grey override
# ------------------------------------------------------------------------------
panel_2group <-
  plot_group_violins(
    tas ~ vviq_group_2,
    data = all_data,
    y_lab = "Total TAS score",
    x_lab = NULL
  ) +
  scale_x_aphantasia(add = c(0.4, 0.7)) +
  # Override plot_group_violins()'s default discrete colour/fill mapping
  # with a uniform grey for BOTH groups — deliberately undifferentiated.
  ggplot2::scale_color_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::scale_fill_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::labs(title = "The common 2-group approach")

# ------------------------------------------------------------------------------
# 4-group panel — scale_discrete_aphantasia()
# ------------------------------------------------------------------------------
panel_4group <-
  plot_group_violins(
    tas ~ vviq_group_4,
    data = all_data,
    y_lab = NULL,  # shared y-axis meaning with panel_2group — avoid
                    # repeating the label when composed side by side
    x_lab = NULL
  ) +
  scale_x_aphantasia(add = c(0.4, 0.7)) +
  scale_discrete_aphantasia() +
  ggplot2::labs(title = "A finer-grained 4-group alternative")

# ------------------------------------------------------------------------------
# Compose side by side
# ------------------------------------------------------------------------------
fig_groups <- 
  panel_2group + panel_4group + 
  patchwork::plot_layout(widths = c(1, 1.6))

save_ggplot(
  "inst/visualisation/paper/fig_baseline_comparison.pdf",
  fig_groups,
  ncol = 2, height = 90)