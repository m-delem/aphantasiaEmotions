# ==============================================================================
# Graphical abstract — SQUARE version, for PCJ article-page display
# ==============================================================================
#
# PCJ's display locks THUMBNAIL WIDTH but not height — a "long" image is fine,
# even advantageous, on the article-list thumbnail; a "wide" image gets shrunk
# illegibly. This version is built square-ish and single-panel
# (floor-group finding only, no 2-group comparison — doesn't fit legibly
# at this aspect ratio) rather than a shrunk copy of the wide version.

devtools::load_all()
library(patchwork)

base_size <- 20
ax_titles <- 0.8
ax_rel_x <- 0.65
ax_rel_y <- 0.7
ax_margins <- 1.25
lw <- 0.1

# ------------------------------------------------------------------------------
# Load the canonical multilevel model directly
# ------------------------------------------------------------------------------
floor_group_additive_multilevel <- readRDS(
  "inst/models/floor_group_additive_multilevel_tot.rds"
)
model_data <- floor_group_additive_multilevel$data

hist_panel <-
  plot_vviq_marginal_histogram(
    model_data, 
    base_size = base_size,
    x_lab = "Visual imagery vividness distribution (VVIQ)",
    floor_linewidth = lw,
    axis_relative_size = 0.8,
    axis_relative_y = ax_rel_y,
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), color = "black", face = "bold",
      margin = ggplot2::margin(b = 1)
    ),
    panel.grid.major.y = ggplot2::element_line(linewidth = lw)
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      size = ggplot2::rel(ax_rel_y), 
      margin = ggplot2::margin(t = ax_margins)),
    axis.text.x.bottom = ggplot2::element_text(
      size = ggplot2::rel(ax_rel_x),
      margin = ggplot2::margin(t = ax_margins)),
    axis.title.y = ggplot2::element_text(
      size = ggplot2::rel(0.65), 
      margin = ggplot2::margin(r = ax_margins)),
    axis.text.y.left = ggplot2::element_text(
      margin = ggplot2::margin(r = ax_margins)),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  scale_x_vviq(
    limits = c(8, 81), 
    expand = ggplot2::expansion(mult = c(0.02, 0))) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = lw)

main_panel <-
  plot_floor_group(
    floor_group_additive_multilevel, model_data,
    x_lab = "Visual imagery vividness continuum",
    y_lab = "Alexithymia score",
    dot_size = 0.1,
    floor_jitter_size = 0.1,
    cross_size = 0.7,
    cross_stroke = 0.2,
    stat_txt_size = 4,
    stat_label_lineheight = 0.2,
    mean_line_width = lw,
    floor_violin_linewidth = lw,
    floor_pointrange_linewidth = lw,
    floor_pointrange_size = 0.05,
    floor_label_color = "transparent",
    floor_guide_color = "transparent",
    arrow_length = 0.02,
    arrow_linewidth = lw,
    tick_linewidth = lw,
    fitted_line_width = lw,
    extrap_line_width = lw,
    base_size = base_size,
    axis_relative_size = 0.8,
    axis_relative_x = ax_rel_x,
    axis_relative_y = ax_rel_y,
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), color = "black", face = "bold",
      margin = ggplot2::margin(b = 1)
    ),
    plot.caption  = ggplot2::element_blank(),
    # plot.caption  = ggplot2::element_text(
    #   size  = ggplot2::rel(ax_rel_x), margin = ggplot2::margin(t = ax_margins)),
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_line(linewidth = lw),
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), 
      margin = ggplot2::margin(t = ax_margins)),
    axis.title.y = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), 
      margin = ggplot2::margin(r = ax_margins)),
    axis.text.x.bottom = ggplot2::element_text(
      margin = ggplot2::margin(t = ax_margins)),
    axis.text.y.left = ggplot2::element_text(
      margin = ggplot2::margin(r = ax_margins)),
    axis.text.y.right = ggplot2::element_text(
      size = 11,
      lineheight = 0.15,
      margin = ggplot2::margin(l = ax_margins)),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  # Salient arrow + label replacing the EOR page's more technical "Floor
  # VVIQ" guide line — a graphical-abstract audience needs the plain-
  # language explanation directly, not just a discrete tick label.
  ggplot2::annotate(
    geom = "curve",
    x = 30, xend = 17, y = 88, yend = 68,
    curvature = -0.3, color = "#8B3A3E", linewidth = 0.2,
    arrow = ggplot2::arrow(length = grid::unit(0.02, "inches"))
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 31, y = 90,
    label = "Complete aphantasia (VVIQ = 16)\nN = 147",
    color = "#8B3A3E", fontface = "bold", hjust = 0, size = 5,
    lineheight = 0.2
  )

square_abstract <-
  hist_panel / main_panel +
  patchwork::plot_layout(heights = c(1, 5)) +
  plot_annotation(
    title = paste(
      "Complete aphantasia, as opposed to weak imagery,\nis linked to",
      "typical meta-emotional functioning"
    ),
    subtitle = paste(
      "People with a complete absence of visual imagery show alexithymia",
      "levels\nindistinguishable from typical imagers \u2014 whereas weak,",
      "residual imagery\nis associated with greater difficulty understanding",
      "one's own emotions."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size, family = "Montserrat",
        margin = ggplot2::margin(b = 2), face = "bold", hjust = 0,
        lineheight = 0.15
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 4, family = "Montserrat",
        margin = ggplot2::margin(b = 4), face = "italic", hjust = 0,
        lineheight = 0.15
      )
    )
  ) &
  ggplot2::theme(plot.margin = ggplot2::margin_auto(1.5, unit = "pt"))

save_ggplot(
  square_abstract,
  path = here::here("man/figures/graphical_abstract_v2_square.png"),
  width = 1000,
  height = 1000,
  dpi = 600,
  units = "px",
  return = TRUE
)
