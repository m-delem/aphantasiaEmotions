devtools::load_all()

library(patchwork)

hist_panel <-
  plot_vviq_marginal_histogram(model_data) +
  # Clean plot separation
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::scale_x_continuous(
    limits = c(8, 81),
    expand = ggplot2::expansion(c(0.02, 0.02))
    )

main_panel <- plot_floor_group(floor_group_additive, model_data)

plot(hist_panel / main_panel + patchwork::plot_layout(heights = c(1, 4)))

save_ggplot(
  "inst/visualisation/paper/fig_floor_model.pdf", 
  ncol = 2, height = 140)
