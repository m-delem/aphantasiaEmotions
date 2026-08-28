# ---------------------------------------------------------------------------- #
# Graphical abstract — WIDE version, for GitHub README and OSF
# ---------------------------------------------------------------------------- #
#
# Two panels: the naive 2-group baseline (grey — "the common approach"), and the
# floor-group model (the actual finding), composed with its own VVIQ marginal
# histogram strip. VVIQ/TAS terminology is replaced throughout with plain
# language labels for a non-specialist graphical-abstract audience
# (EOR pages keep the technical VVIQ/TAS labels).

devtools::load_all()
library(patchwork)

base_size <- 20
ax_titles <- 0.8
ax_rel_x <- 0.65
ax_rel_y <- 0.7
ax_margins <- 1.25
lw <- 0.1

# ---------------------------------------------------------------------------- #
# Load the canonical multilevel model directly ----
# ---------------------------------------------------------------------------- #
floor_group_additive_multilevel <- readRDS(
  "inst/models/floor_group_additive_multilevel_tot.rds"
)
model_data <- floor_group_additive_multilevel$data

# ---------------------------------------------------------------------------- #
# Left panel: the common approach ----
# Left panel: the naive 2-group baseline, grey, "the common approach"
# ---------------------------------------------------------------------------- #
baseline_panel <-
  plot_group_violins(
    tas ~ vviq_group_2,
    title = "The common approach",
    x_lab = "Visual imagery group",
    y_lab = "Alexithymia score",
    dot_size = 0.1,
    middle.linewidth = 0.3,
    base_size = base_size,
    axis_relative_size = 1,
    axis_relative_x = ax_rel_x,
    axis_relative_y = ax_rel_y,
    text = ggplot2::element_text(lineheight = 0.2),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), color = "grey50", face = "bold",
      margin = ggplot2::margin(b = 1)
    ),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      color = "grey50", fill = NA, linewidth = lw)
  ) +
  ggplot2::theme(
    axis.title.x.bottom = ggplot2::element_text(
      size = ggplot2::rel(ax_titles),
      margin = ggplot2::margin(t = ax_margins)),
    axis.title.y = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), 
      margin = ggplot2::margin(r = ax_margins)),
    axis.text.x.bottom = ggplot2::element_text(
      hjust = 0.25,
      margin = ggplot2::margin(t = ax_margins)),
    axis.text.y.left = ggplot2::element_text(
      margin = ggplot2::margin(r = ax_margins)),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_color_manual(
    values = c("grey50", "grey50"), guide = "none", name = NULL) +
  ggplot2::scale_fill_manual(
    values = c("grey50", "grey50"), guide = "none", name = NULL) +
  ggplot2::scale_y_continuous(
    limits = c(18, 96), breaks = seq(0, 100, by = 10),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "aphantasia" = "Aphantasics\n(VVIQ \u2264 32)\nN = 288", 
      "typical" = "Imagers\n(VVIQ > 32)\nN = 1190"),
    expand = ggplot2::expansion(mult = 0, add = c(0.4, 0.7))
  )

# ---------------------------------------------------------------------------- #
# Progression arrow ----
# Progression arrow: axis-free ggplot containing only a horizontal arrow
# ---------------------------------------------------------------------------- #
mid_arrow <-
  ggplot2::ggplot() +
  ggplot2::annotate(
    geom = "segment",
    x = 0, xend = 1, y = 0.5, yend = 0.5,
    arrow = ggplot2::arrow(
      length = grid::unit(0.02, "inches"), 
      type = "closed"),
    linewidth = 0.2, color = "grey40"
    ) +
  ggplot2::xlim(0, 1) + 
  ggplot2::ylim(0, 1) +
  ggplot2::theme_void()

# ---------------------------------------------------------------------------- #
# Right panel: the floor-group finding ----
# Right panel: the floor-group finding, with its marginal-histogram strip,
# reusing the real plot_floor_group() function (not a simplified rebuild —
# the floor-effect stats and X-bar annotation are worth keeping).
# ---------------------------------------------------------------------------- #
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
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = lw) +
  ggplot2::labs(title = "The complete picture")

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
    x = 33, xend = 17, y = 83, yend = 68,
    curvature = -0.3, color = "#8B3A3E", linewidth = 0.2,
    arrow = ggplot2::arrow(length = grid::unit(0.02, "inches"))
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 31, y = 85,
    label = "Complete aphantasia (VVIQ = 16)\nN = 147",
    color = "#8B3A3E", fontface = "bold", 
    hjust = 0, vjust = 0,  
    size = 5, lineheight = 0.2
  )

# ---------------------------------------------------------------------------- #
# Compose with patchwork's amazing "design" layout tool ----
# ---------------------------------------------------------------------------- #
wide_abstract <-
  baseline_panel + mid_arrow + hist_panel + main_panel +
  plot_layout(
    design = c(
      area(t=2, b=9,  l=1, r=4), # baseline
      area(t=3, b=3,  l=5, r=5), # arrow
      area(t=1, b=2,  l=6, r=15), # hist
      area(t=3, b=10, l=6, r=15)  # main
    )) +
  plot_annotation(
    title = paste(
      "Complete aphantasia, as opposed to weak imagery, is linked to",
      "typical alexithymia levels"
    ),
    subtitle = paste(
      "People with a complete absence of visual imagery show alexithymia",
      "scores indistinguishable from typical imagers\n\u2014 whereas weak,",
      "residual imagery is associated with greater difficulty understanding",
      "one's own emotions."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size, family = "Montserrat",
        margin = ggplot2::margin(b = 1.5), face = "bold", hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 3, family = "Montserrat",
        margin = ggplot2::margin(b = 3), face = "italic", hjust = 0,
        lineheight = 0.15
      )
    )
  ) &
  ggplot2::theme(plot.margin = ggplot2::margin_auto(1.5, unit = "pt"))

save_ggplot(
  wide_abstract,
  path = here::here("man/figures/graphical_abstract_v2_wide.png"),
  width = 1600,
  height = 1000,
  dpi = 600,
  units = "px",
  return = TRUE
)
