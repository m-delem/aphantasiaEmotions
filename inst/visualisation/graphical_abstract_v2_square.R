# ==============================================================================
# Graphical abstract — SQUARE version, for PCJ article-page display
# ==============================================================================
#
# PCJ's display locks THUMBNAIL WIDTH but not height (confirmed by
# inspecting live PCJ article pages) — a "long" image is fine, even
# advantageous, on the article-list thumbnail; a "wide" image gets shrunk
# illegibly. This version is built square-ish and single-panel
# (floor-group finding only, no 2-group comparison — doesn't fit legibly
# at this aspect ratio) rather than a shrunk copy of the wide version.
#
# Self-contained: loads the package and the canonical multilevel model
# directly, no assumed session state.

devtools::load_all()
library(patchwork)

base_size <- 20

floor_group_additive_multilevel <- readRDS(
  system.file("models", "floor_group_additive_multilevel_tot.rds", package = "aphantasiaEmotions")
)

hist_panel <-
  plot_vviq_marginal_histogram(all_data, base_size = base_size * 0.6) +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  ggplot2::scale_x_continuous(limits = c(8, 81), expand = ggplot2::expansion(c(0.02, 0.02)))

main_panel <-
  plot_floor_group(
    floor_group_additive_multilevel, all_data,
    x_lab = "Visual imagery vividness (16 = no imagery)",
    y_lab = "Alexithymia score",
    base_size = base_size
  ) +
  ggplot2::annotate(
    geom = "curve",
    x = 30, xend = 17, y = 88, yend = 68,
    curvature = -0.3, color = "#8B3A3E", linewidth = 0.6,
    arrow = ggplot2::arrow(length = grid::unit(0.12, "inches"))
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 32, y = 90,
    label = "Complete aphantasia\n(N = 147)",
    color = "#8B3A3E", fontface = "bold", hjust = 0, size = 5,
    lineheight = 0.85
  )

# ------------------------------------------------------------------------------
# TRIM PASS, per the "reuse the real function, remove manually if too much"
# plan: this square version has no room for plot_floor_group()'s own
# caption (above-floor slope stats) alongside a title/subtitle stacked
# above it — dropped here. The colour legend is also dropped (redundant
# with the histogram's own colour strip directly above it in this
# composition). Both are still present in the wide version.
# ------------------------------------------------------------------------------
main_panel <- main_panel +
  ggplot2::labs(caption = NULL) +
  ggplot2::theme(legend.position = "none")

p <-
  hist_panel / main_panel +
  patchwork::plot_layout(heights = c(1, 4)) +
  plot_annotation(
    title = paste(
      "Complete aphantasia, as opposed to weak imagery, is linked to",
      "typical meta-emotional functioning"
    ),
    subtitle = paste(
      "People with a complete absence of visual imagery show alexithymia",
      "levels indistinguishable from typical imagers\n\u2014 whereas weak,",
      "residual imagery is associated with greater difficulty understanding",
      "one's own emotions."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size, family = "Montserrat",
        margin = ggplot2::margin(b = 1.5), face = "bold", hjust = 0,
        lineheight = 0.9
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 4, family = "Montserrat",
        margin = ggplot2::margin(b = 3), face = "italic", hjust = 0,
        lineheight = 0.15
      )
    )
  ) &
  ggplot2::theme(plot.margin = ggplot2::margin_auto(1, unit = "pt"))

save_ggplot(
  p,
  path = here::here("man/figures/graphical_abstract_square.png"),
  width = 1000,
  height = 1000,
  dpi = 600,
  units = "px",
  return = TRUE
)

# ------------------------------------------------------------------------------
# THINGS TO CHECK once run against real data:
# 1. Title/subtitle at this narrower width will almost certainly wrap
#    differently (more lines) than the wide version — the manual \n breaks
#    in the paste() calls above were written for the wide layout's line
#    width and likely need re-tuning here rather than reused as-is.
# 2. Given PCJ locks width but not height, consider whether this actually
#    needs to be exactly 1000x1000 (truly square) or whether a taller
#    format (e.g. 1000x1300) would use the "long images are fine" PCJ
#    behaviour better while still being far more compact than the wide
#    5:2-ish version — worth a real comparison once both render.
# ------------------------------------------------------------------------------
