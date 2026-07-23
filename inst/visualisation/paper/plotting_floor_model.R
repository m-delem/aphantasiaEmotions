devtools::load_all()

library(patchwork)

floor_group_additive_multilevel <-
  readRDS("inst/models/floor_group_additive_multilevel_tot.rds")
model_data <- floor_group_additive_multilevel$data  # already has complete_aphant, study, vviq, tas


hist_panel <-
  plot_vviq_marginal_histogram(model_data) +
  # Clean plot separation
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::scale_x_continuous(
    limits = c(7.9, 81),
    expand = ggplot2::expansion(c(0.02, 0.02))
    )

main_panel <- 
  plot_floor_group(
    floor_group_additive_multilevel, 
    model_data,
    vviq_breaks = seq(16, 80, 16),
    xbar_label = expression(bar(x)),
    xbar_vjust = 0.3,
    limits = c(7.9, 81),
    stat_label_x = 7.9,
    floor_label_x = 15,
    floor_label_size = 1.5
  ) +
  ggplot2::theme(
    axis.text.y.right = ggplot2::element_text(size = 10)
  )

plot(hist_panel / main_panel + patchwork::plot_layout(heights = c(1, 4)))

save_ggplot(
  "inst/visualisation/paper/fig_floor_model_1col.pdf", 
  ncol = 1, height = 130)
