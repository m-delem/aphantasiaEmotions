# ==============================================================================
# Segmented model figure — fitted curve, Kvamme reference lines, knot
# posterior half-eye
# ==============================================================================
# Requires: ggdist (for stat_halfeye), already-fitted segmented_estimated,
# and the validated Kvamme replication coefficients from
# kvamme_analysis_replication.R (sourced below).

devtools::load_all()

segmented_estimated <- readRDS("inst/analysis/models_comparison/segmented_estimated_knot_tot.rds")
source("inst/analysis/kvamme_analysis_replication.R")  # provides kvamme_aphant_grid, kvamme_typical_grid

# ------------------------------------------------------------------------------
# Segmented model's fitted curve — same mechanism already validated in
# plotting_model_overlay.R and probe_segmented_prediction_curve.R.
# ------------------------------------------------------------------------------
pred_grid <- data.frame(vviq = seq(16, 80, length.out = 200))
pred_segmented <- as.data.frame(marginaleffects::predictions(segmented_estimated, newdata = pred_grid))

# ------------------------------------------------------------------------------
# Knot posterior draws, for the half-eye — same extraction as
# parameter_evidence.R.
# ------------------------------------------------------------------------------
knot_draws <- brms::as_draws_df(segmented_estimated, variable = "b_k_Intercept")

# ------------------------------------------------------------------------------
# y-position for the half-eye: placing it just above the curve's peak (where
# there's naturally empty space, since the curve peaks near the knot
# itself) to kept it close to the feature it describes.
# ------------------------------------------------------------------------------
halfeye_y_position <- max(pred_segmented$estimate) + 1.5

# ------------------------------------------------------------------------------
# Assemble the plot
# ------------------------------------------------------------------------------
p_segmented <-
  ggplot2::ggplot() +
  # Raw scatter, faint 
  ggplot2::geom_point(
    data = all_data,
    ggplot2::aes(x = vviq, y = tas),
    alpha = 0.1, size = 0.8, color = "grey60"
  ) +
  # Kvamme et al.'s two reference lines, restricted to their own real VVIQ
  # ranges (no extrapolation) — validated exact match to their reported
  # r-values (0.186 / -0.236), so captioned as their actual analysis, not
  # an approximation.
  ggplot2::geom_line(
    data = kvamme_aphant_grid,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = 0.3, linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = kvamme_typical_grid,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = 0.3, linetype = "dashed"
  ) +
  # Segmented model's own fitted curve — the figure's main subject
  ggplot2::geom_line(
    data = pred_segmented,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#009E73", linewidth = 0.5
  ) +
  # Kvamme's fixed threshold (32), as a vertical dotted reference —
  # directly comparable to the knot's estimated posterior below
  ggplot2::geom_vline(
    xintercept = 32,
    linetype = "dotted", color = "#377EB8", linewidth = 0.4
  ) +
  # Knot posterior half-eye
  ggdist::stat_halfeye(
    data = data.frame(knot = knot_draws$b_k_Intercept),
    ggplot2::aes(x = knot, y = halfeye_y_position),
    orientation = "horizontal",
    side = "top",
    height = 6,     # modest height so it doesn't dominate — tune to taste
    fill = "#009E73", slab_alpha = 0.5, 
    size = 0.5,
    linewidth = 0.1,
    point_interval = "median_qi",
    .width = 0.95
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 33, y = 81,
    label = "Kvamme et al.'s\nfixed threshold",
    color = "#377EB8", size = 2, hjust = 0, vjust = 0
  ) +
  ggplot2::labs(
    x = "VVIQ score",
    y = "Total TAS score",
    caption = sprintf(
      "Estimated knot: %.1f [%.1f, %.1f]",
      stats::median(knot_draws$b_k_Intercept),
      stats::quantile(knot_draws$b_k_Intercept, 0.025),
      stats::quantile(knot_draws$b_k_Intercept, 0.975)
    )
  ) +
  scale_x_vviq(breaks = seq(16, 80, by = 8)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.02, 0))) +
  theme_pdf(
    base_theme = ggplot2::theme_minimal,
    panel.grid.minor = ggplot2::element_blank()
  )

save_ggplot(
  "inst/visualisation/paper/fig_segmented_knot.pdf", p_segmented, 
  ncol = 1, height = 110)

plot(p_segmented)