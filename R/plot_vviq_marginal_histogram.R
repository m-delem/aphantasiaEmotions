# ==============================================================================
# VVIQ marginal distribution plot — standalone, composable with
# plot_floor_group() via patchwork
# ==============================================================================
#
# DESIGN RATIONALE: this visualises the empirical
# fact that originally motivated the floor-group modelling approach — VVIQ's
# pooled distribution shows a sharp, isolated spike at the floor (VVIQ=16)
# plus a more continuous, if irregular, remainder from ~20-80. A histogram
# (not a violin/density) is used deliberately, to visually distinguish this
# from plot_floor_group()'s violin (which shows TAS's distribution WITHIN
# the floor group) — this plot shows VVIQ's OWN distribution across
# EVERYONE, a different variable and a different claim. Floor bar coloured
# to match plot_floor_group()'s violin (#C44E52) for a consistent visual
# vocabulary when composed together; the rest use the same viridis mapping
# as plot_floor_group()'s scatter, reinforcing "this part is one continuum"
# visually across both panels.
#
# COMPOSITION: designed to be stacked ABOVE plot_floor_group() via
# patchwork (`plot_vviq_marginal_histogram(data) / plot_floor_group(model, data)`),
# sharing the same x-axis breaks/range so the two panels align. Deliberately
# kept as a SEPARATE function (not a flag inside plot_floor_group()) so:
#   (a) plot_floor_group() always returns a plain, `+`-composable ggplot
#       object, matching every other function in ggplot_tools.R — no
#       special-casing for a patchwork-vs-ggplot return type;
#   (b) this histogram is independently reusable (e.g. for EOR narrative
#       content about the floor-spike discovery, unconnected to this
#       specific model).

#' Plot the marginal VVIQ distribution (floor spike + continuous remainder)
#'
#' @description
#' Shows the empirical VVIQ distribution across the whole sample as a
#' histogram, with the floor bin (VVIQ=16) coloured to match
#' `plot_floor_group()`'s violin, and the remainder coloured by a viridis
#' gradient matching that function's scatter — visually establishing the
#' "floor spike + continuum" pattern that motivates the floor-group model.
#'
#' @param data The data frame containing `vviq` and `complete_aphant`
#' (must match the same definition used to fit floor_group_additive).
#' @param binwidth Histogram bin width, in VVIQ-scale units. Default 2.
#' @param x_lab Label for the x-axis. Omitted by default, the plot is most often
#' meant to sit above the floor-group model's plot.
#' @param y_lab Label for the y-axis. 
#' @param vviq_breaks x-axis breaks — MUST MATCH plot_floor_group()'s
#' `vviq_breaks` argument if composing the two via patchwork, or the panels
#' will misalign. Default `seq(16, 80, 4)`, matching plot_floor_group()'s
#' own default.
#' @param base_theme A ggplot2 theme function. Default `ggplot2::theme_minimal`.
#' @param axis_relative_size Relative size of the axis text. Default is 1.
#' @param axis_relative_y Relative size of the y-axis text. Default is 0.85.
#' @param col_width_prop Proportion of `binwidth` used as each column's
#' plotted width (leaves a small gap between columns). Default 0.9.
#' @param floor_fill_color Fill colour for the floor bin (VVIQ=16). Default
#' is "#C44E52" — matches [plot_floor_group()]'s violin fill by default;
#' keep these in sync if composing the two panels together.
#' @param floor_line_color Border colour for the floor bin. Default is
#' "#8B3A3E" — matches [plot_floor_group()]'s violin border by default.
#' @param floor_linewidth Border line width for the floor bin. Default is
#' 0.2.
#' @param ... Additional arguments passed to `theme_pdf()`.
#'
#' @returns A ggplot2 object (a plain histogram, NOT a patchwork composite —
#' compose with plot_floor_group() yourself, e.g. via `/` from patchwork).
#' @export
plot_vviq_marginal_histogram <- function(
    data,
    binwidth = 1,
    x_lab = NULL,
    y_lab = "Participant count",
    vviq_breaks = seq(16, 80, 4),
    base_theme = ggplot2::theme_minimal,
    axis_relative_size = 1,
    axis_relative_y = 0.85,
    col_width_prop = 0.9,
    floor_fill_color = "#C44E52",
    floor_line_color = "#8B3A3E",
    floor_linewidth = 0.2,
    ...
) {
  # NOTE: this function's default output is designed to compose with
  # plot_floor_group() via patchwork. In practice, composition has required
  # forcing scale_x_continuous(limits = c(8, 81)) on THIS panel's output
  # (added at the call site, not inside this function) to align with hand-
  # tuned annotation positions inside plot_floor_group(). If this
  # function's own default x-expansion changes, re-check that alignment
  # still holds in the composed figure.
  # ----------------------------------------------------------------------
  # Bin the data manually rather than letting geom_histogram() do it
  # internally, so the floor bin can be coloured differently from the rest
  # without relying on a fill-by-group aesthetic that might not align
  # cleanly with arbitrary bin edges (VVIQ=16 needs to be its OWN bin,
  # not blended with 17-18 into a shared bin — manual breaks guarantee this).
  # ----------------------------------------------------------------------
  bin_breaks <- seq(16, 80 + binwidth, by = binwidth)
  data$vviq_bin <- cut(
    data$vviq, 
    breaks = bin_breaks, 
    right = FALSE, 
    include.lowest = TRUE)
  
  bin_counts <- stats::aggregate(
    list(count = data$vviq),
    by = list(vviq_bin = data$vviq_bin),
    FUN = length
  )
  # Bin midpoints for plotting and for the viridis colour mapping
  bin_edges <- bin_breaks[-length(bin_breaks)]
  names(bin_edges) <- levels(data$vviq_bin)
  bin_counts$vviq_mid <- 
    bin_edges[as.character(bin_counts$vviq_bin)] + binwidth / 2
  
  bin_counts$is_floor <- bin_counts$vviq_mid < (16 + binwidth)
  
  # ----------------------------------------------------------------------
  # Assemble the plot
  # ----------------------------------------------------------------------
  p <-
    ggplot2::ggplot() +
    # Above-floor bins: viridis gradient by vviq midpoint, matching
    # plot_floor_group()'s scatter colour mapping
    ggplot2::geom_col(
      data = bin_counts[!bin_counts$is_floor, ],
      ggplot2::aes(x = .data$vviq_mid, y = .data$count, fill = .data$vviq_mid),
      width = binwidth * col_width_prop
    ) +
    ggplot2::scale_fill_viridis_c(guide = "none") +  # no legend here — the
    # composed panel below already has one
    # Floor bin: fixed colour matching plot_floor_group()'s violin
    ggplot2::geom_col(
      data = bin_counts[bin_counts$is_floor, ],
      ggplot2::aes(x = .data$vviq_mid, y = .data$count),
      fill = floor_fill_color, 
      color = floor_line_color, 
      linewidth = floor_linewidth,
      width = binwidth * col_width_prop
    ) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::scale_x_continuous(breaks = vviq_breaks) +
    theme_pdf(
      base_theme = base_theme,
      axis_relative_size = axis_relative_size,
      axis_relative_y = axis_relative_y,
      axis.text.x = ggplot2::element_blank(),   # redundant if stacked above
      # plot_floor_group() — remove
      # this theme override if
      # using standalone
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      ...
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# EXAMPLE COMPOSITION (requires patchwork):
#
#   library(patchwork)
#   hist_panel <- plot_vviq_marginal_histogram(model_data)
#   main_panel <- plot_floor_group(floor_group_additive, model_data)
#   hist_panel / main_panel + patchwork::plot_layout(heights = c(1, 3))
#
# NOTE: if using standalone (not composed), you'll likely want to REMOVE
# the axis.text.x = element_blank() / axis.ticks.x = element_blank()
# overrides above, since there'd be no panel below to carry the x-axis
# labels.
#
# THINGS TO CHECK once run against real data:
# 1. patchwork must be installed separately — not added as a hard
#    dependency of this function itself, since the function returns a
#    plain ggplot and composition is left to the caller (per the Option B
#    design decision).
# 2. Vertical alignment of the two panels' PLOT AREAS (not just axis
#    ticks) when composed depends on patchwork handling differing legend/
#    label widths between the two — the "no legend" choice on this
#    histogram was made specifically to avoid width mismatches with
#    plot_floor_group()'s colorbar legend; if plot_floor_group()'s legend
#    width changes, re-check this alignment.
# ------------------------------------------------------------------------------