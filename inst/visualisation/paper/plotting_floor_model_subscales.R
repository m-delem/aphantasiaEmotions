devtools::load_all()

library(patchwork)

m_tot <- readRDS("inst/models/floor_group_additive_multilevel_tot.rds")
m_dif <- readRDS("inst/models/floor_group_additive_multilevel_dif.rds")
m_ddf <- readRDS("inst/models/floor_group_additive_multilevel_ddf.rds")
m_eot <- readRDS("inst/models/floor_group_additive_multilevel_eot.rds")

box_color <- "grey50"
dot_size <- 0.5
vviq_limits <- c(3.6, 81)
vviq_breaks <- seq(16, 80, 16)
tas_breaks <- scales::pretty_breaks(5)
stat_x <- 4.5
arr_x <- 9.3
cap_rel <- 0.77
cross_size <- 1.5
cross_stroke <- 0.4
floor_dot <- 0.3
floor_lw <- 0.3
fitted_lw <- 0.4

p_dif <-
  plot_floor_group(
    m_dif, m_dif$data, y_lab = "TAS DIF score",
    fitted_line_width = fitted_lw,
    dot_size = dot_size,
    floor_jitter_size = dot_size,
    floor_pointrange_size = floor_dot,
    floor_pointrange_linewidth = floor_lw,
    cross_size = cross_size,
    cross_stroke = cross_stroke,
    stat_label_x = stat_x,
    arrow_x = arr_x,
    limits = vviq_limits,
    tas_breaks = tas_breaks,
    vviq_breaks = vviq_breaks,
    xbar_label = NULL,
    floor_label_size = 0,
    floor_guide_color = "transparent",
    legend.position = "none",
    panel.border = ggplot2::element_rect(
      color = box_color, fill = NULL, linewidth = 0.2),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(cap_rel))
  ) +
  ggplot2::scale_y_continuous(limits = c(7, 35)) +
  ggplot2::labs(
    title = "Difficulty Identifying Feelings",
    x = NULL,
    y = "TAS-20 sub-scale score"
    )

p_ddf <-
  plot_floor_group(
    m_ddf, m_ddf$data, y_lab = "TAS DDF score",
    fitted_line_width = fitted_lw,
    dot_size = dot_size,
    floor_jitter_size = dot_size,
    floor_pointrange_size = floor_dot,
    floor_pointrange_linewidth = floor_lw,
    cross_size = cross_size,
    cross_stroke = cross_stroke,
    stat_label_x = stat_x,
    arrow_x = arr_x,
    limits = vviq_limits,
    tas_breaks = tas_breaks,
    vviq_breaks = vviq_breaks,
    xbar_label = NULL,
    floor_label_size = 0,
    floor_guide_color = "transparent",
    legend.position = "none",
    panel.border = ggplot2::element_rect(
      color = box_color, fill = NULL, linewidth = 0.2),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(cap_rel))
  ) +
  ggplot2::scale_y_continuous(limits = c(5, 25)) +
  ggplot2::labs(title = "Difficulty Describing Feelings", y = NULL)

p_eot <-
  plot_floor_group(
    m_eot, m_eot$data, y_lab = "TAS EOT score",
    fitted_line_width = fitted_lw,
    dot_size = dot_size,
    floor_jitter_size = dot_size,
    floor_pointrange_size = floor_dot,
    floor_pointrange_linewidth = floor_lw,
    cross_size = cross_size,
    cross_stroke = cross_stroke,
    stat_label_x = stat_x,
    arrow_x = arr_x,
    limits = vviq_limits,
    tas_breaks = tas_breaks,
    vviq_breaks = vviq_breaks,
    xbar_label = NULL,
    floor_label_size = 0,
    floor_guide_color = "transparent",
    legend.position = "none",
    panel.border = ggplot2::element_rect(
      color = box_color, fill = NULL, linewidth = 0.2),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(cap_rel))
  ) +
  ggplot2::scale_y_continuous(limits = c(8, 36)) +
  ggplot2::labs(title = "Externally-Oriented Thinking", x = NULL, y = NULL)

p_subs <- p_dif + p_ddf + p_eot

save_ggplot(
  "inst/visualisation/paper/fig_floor_model_subscales.pdf", 
  p_subs,
  return = TRUE,
  ncol = 2, 
  height = 70
  )
