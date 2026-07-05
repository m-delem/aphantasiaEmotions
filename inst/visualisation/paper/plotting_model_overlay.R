# ==============================================================================
# Linear / GAM / segmented overlay — model comparison figure
# ==============================================================================
devtools::load_all()

lm_linear           <- readRDS("inst/analysis/models_comparison/lm_linear_tot.rds")
gam_tot             <- readRDS("inst/analysis/models_comparison/gam_tot.rds")
segmented_estimated <- readRDS("inst/analysis/models_comparison/segmented_estimated_knot_tot.rds")

# ------------------------------------------------------------------------------
# Prediction curves, one consistent mechanism (marginaleffects::predictions())
# across all three models, on the same vviq grid.
# ------------------------------------------------------------------------------
pred_grid <- data.frame(vviq = seq(16, 80, length.out = 200))

pred_linear    <- as.data.frame(marginaleffects::predictions(lm_linear, newdata = pred_grid))
pred_gam       <- as.data.frame(marginaleffects::predictions(gam_tot, newdata = pred_grid))
pred_segmented <- as.data.frame(marginaleffects::predictions(segmented_estimated, newdata = pred_grid))

pred_linear$model    <- "Linear"
pred_gam$model       <- "GAM"
pred_segmented$model <- "Segmented"

all_preds <- rbind(
  pred_linear[, c("vviq", "estimate", "model")],
  pred_gam[, c("vviq", "estimate", "model")],
  pred_segmented[, c("vviq", "estimate", "model")]
)

model_colors <- c(
  "Linear"    = "grey40",
  "GAM"       = "#E69F00",
  "Segmented" = "#009E73"
)

# End-of-line label positions: rightmost point of each curve. Relies on
# split()'s group names being preserved as row names through
# do.call(rbind, ...) — a real but common R idiom; if the grouping
# variable (all_preds$model) or its levels change, the ["GAM",] indexing
# below would silently break or mistarget rather than error, so double
# check this still points at the right row if the model set changes.
end_labels <- do.call(rbind, lapply(split(all_preds, all_preds$model), function(d) {
  d[which.max(d$vviq), ]
}))
# GAM's end-of-line label nudged down slightly — at vviq=80 the GAM and
# Segmented curves sit close enough that their text labels would overlap
# without this offset.
end_labels["GAM",]$estimate <- end_labels["GAM",]$estimate - 1.5

# ------------------------------------------------------------------------------
# LOO comparison stats for the caption (hardcoded from the already-computed
# comparison table — CONSIDER replacing with a live read of
# comparison_all_models_tot.rds if you want this to stay auto-updating;
# left as hardcoded for now since the exact wording/rounding is easy to
# hand-tune here and the comparison is unlikely to change once locked).
# The real table:
#   comp_table <- readRDS(
#     "inst/analysis/models_comparison/comparison_all_models_tot.rds")
# ------------------------------------------------------------------------------
overlay_caption <- "elpd (relative to segmented): GAM \u22124.6 (SE 2.3); linear \u221222.4 (SE 7.5)"

# ------------------------------------------------------------------------------
# Assemble the plot
# ------------------------------------------------------------------------------
p_overlay <-
  ggplot2::ggplot() +
  # Raw scatter, faint 
  ggplot2::geom_point(
    data = all_data,
    ggplot2::aes(x = vviq, y = tas),
    alpha = 0.1, size = 0.8, color = "grey60"
  ) +
  ggplot2::geom_line(
    data = all_preds,
    ggplot2::aes(x = vviq, y = estimate, color = model),
    linewidth = 0.5
  ) +
  ggplot2::geom_text(
    data = end_labels,
    ggplot2::aes(x = vviq, y = estimate, label = model, color = model),
    hjust = 0, nudge_x = 1, size = 2, fontface = "bold"
  ) +
  ggplot2::scale_color_manual(values = model_colors, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = seq(16, 80, 8),
    # limits/expansion tuned to fit end-of-line labels (esp. "Segmented",
    # the longest) within a single manuscript column without excess empty
    # margin — this replaced an earlier, wider version once single-column
    # width made that margin look empty; re-check if labels or column
    # width change.
    limits = c(16, 93),
    expand = ggplot2::expansion(c(0, 0))
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.02, 0))) +
  ggplot2::labs(
    x = "VVIQ score",
    y = "Total TAS score",
    caption = overlay_caption
  ) +
  theme_pdf(
    base_theme = ggplot2::theme_minimal,
    panel.grid.minor = ggplot2::element_blank()
  )

save_ggplot(
  "inst/visualisation/paper/fig_model_overlay.pdf", p_overlay,
  ncol = 1, height = 110)

plot(p_overlay)

# ------------------------------------------------------------------------------
# SUPERSEDED ALTERNATIVE, kept for reference: stacking this overlay below
# the 2-group/4-group baseline-comparison figure (plotting_baseline_
# comparison.R), with a combined 4-model caption. Dropped because the two
# panels have different observation-visibility (violins show raw points,
# this overlay's curves didn't at the time) and different plot grammars
# (raincloud violins vs. line curves). Replaced by
# the side-by-side pairing with plotting_segmented_knot.R below instead,
# which shares a consistent grammar (line plots + matching scatter) across
# both panels.
# ------------------------------------------------------------------------------
# source("inst/visualisation/paper/plotting_baseline_comparison.R")
# 
# overlay_caption <- "elpd (relative to segmented): GAM \u22124.6 (SE 2.3); linear \u221222.4 (SE 7.5); 4-groups \u221229.7 (SE 8.2); 2-groups \u221248.5 (SE 10.2)"
# 
# fig_continuous <- p +
#   ggplot2::labs(
#     title = "Better fitting continuous models",
#     caption = overlay_caption
#   ) + 
#   ggplot2::theme(plot.margin = ggplot2::margin(t = 20))
# 
# fig_groups / fig_continuous + plot_layout(heights = c(1, 1.6))
# save_ggplot(
#   "inst/visualisation/paper/fig_model_comparison.pdf", 
#   ncol = 2, height = 150
# )

# ------------------------------------------------------------------------------
# FINAL: side-by-side composition with the segmented-knot figure.
#
# IMPORTANT FRAMING NOTE: these two panels make DIFFERENT KINDS of
# comparisons, despite sharing a visual grammar (line plots, matching
# scatter) that makes them look like a natural pair. The left panel
# (this one) is an INTERNAL, methodological comparison — which of OUR
# candidate models fits best, via LOO. The right panel
# (plotting_segmented_knot.R) is an EXTERNAL, substantive comparison —
# how does OUR estimated knot relate to Kvamme et al.'s published, fixed
# threshold. Visual coherence between the two panels is real and
# deliberate, but the manuscript prose introducing this figure MUST make
# this distinction explicit, or a reader could conflate a technical
# modelling choice (GAM vs. segmented) with a scientific claim about the
# literature (our boundary vs. theirs). Titles below are a first pass;
# consider more explicitly distinguishing wording, e.g. "Choosing among
# our models" (left) vs. "Testing an external threshold" (right), so the
# distinction is visible in the titles themselves, not just in prose.
# ------------------------------------------------------------------------------
source("inst/visualisation/paper/plotting_segmented_knot.R")
library(patchwork)

(p_overlay + 
    ggplot2::ggtitle("Choosing among our candidate models") +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = -1))) + 
  (p_segmented + ggplot2::ggtitle("Testing Kvamme et al.'s (2026) threshold")) + 
  plot_layout(axis_titles = "collect")

save_ggplot(
  "inst/visualisation/paper/fig_model_comparison.pdf",
  ncol = 2, height = 110)
