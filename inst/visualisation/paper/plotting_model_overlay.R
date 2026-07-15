# ==============================================================================
# Linear / GAM / segmented / floor-group / Kvamme overlay — model comparison 
# figure
# ==============================================================================
devtools::load_all()

lm_linear           <- readRDS("inst/models/lm_linear_tot.rds")
gam_tot             <- readRDS("inst/models/gam_tot.rds")
segmented_estimated <- readRDS("inst/models/segmented_estimated_knot_tot.rds")
floor_group_model   <- readRDS("inst/models/floor_group_additive_multilevel_tot.rds")

kvamme_data    <- all_data |> dplyr::filter(study == "kvamme")
kvamme_aphant  <- kvamme_data |> dplyr::filter(vviq_group_2 == "aphantasia")
kvamme_typical <- kvamme_data |> dplyr::filter(vviq_group_2 != "aphantasia")

lm_kvamme_aphant  <- lm(tas ~ vviq, data = kvamme_aphant)
lm_kvamme_typical <- lm(tas ~ vviq, data = kvamme_typical)

# ------------------------------------------------------------------------------
# Prediction curves, one consistent mechanism (marginaleffects::predictions())
# across all four models, on the same vviq grid.
# ------------------------------------------------------------------------------
pred_grid <- 
  data.frame(vviq = seq(16, 80, length.out = 200)) |> 
  dplyr::mutate(
  complete_aphant = 
    ifelse(.data$vviq == 16, "floor", "above_floor") |> 
    factor(levels = c("above_floor", "floor"))
  )

pred_linear    <- as.data.frame(
  marginaleffects::predictions(lm_linear, newdata = pred_grid))
pred_gam       <- as.data.frame(
  marginaleffects::predictions(gam_tot, newdata = pred_grid))
pred_segmented <- as.data.frame(
  marginaleffects::predictions(segmented_estimated, newdata = pred_grid))
pred_floor     <- as.data.frame(
  marginaleffects::predictions(
    floor_group_model, newdata = pred_grid, re_formula = NA))

# ------------------------------------------------------------------------------
# Kvamme's predictions are a composite of two model predictions
# ------------------------------------------------------------------------------
pred_kvamme_aphant  <- as.data.frame(
  marginaleffects::predictions(lm_kvamme_aphant, newdata = pred_grid)) |> 
  dplyr::filter(vviq <= 32)
pred_kvamme_typical  <- as.data.frame(
  marginaleffects::predictions(lm_kvamme_typical, newdata = pred_grid))|> 
  dplyr::filter(vviq > 32)
pred_kvamme <- rbind(pred_kvamme_aphant, pred_kvamme_typical)

pred_linear$model    <- "Linear"
pred_gam$model       <- "GAM"
pred_segmented$model <- "Segmented"
pred_floor$model     <- "Floor-group"
# pred_kvamme$model    <- "Kvamme"

all_preds <- rbind(
  pred_linear[, c("vviq", "estimate", "model")],
  pred_gam[, c("vviq", "estimate", "model")],
  pred_segmented[, c("vviq", "estimate", "model")],
  pred_floor[, c("vviq", "estimate", "model")]
  # pred_kvamme[, c("vviq", "estimate", "model")]
)

model_colors <- c(
  "Linear"      = "grey40",
  "GAM"         = "#E69F00",
  "Segmented"   = "#009E73",
  "Floor-group" = "#8B3A3E",
  "Kvamme"      = "#377EB8"
)

# End-of-line label positions: rightmost point of each curve. Relies on
# split()'s group names being preserved as row names through
# do.call(rbind, ...) — a real but common R idiom; if the grouping
# variable (all_preds$model) or its levels change, the ["GAM",] indexing
# below would silently break or mistarget rather than error, so double
# check this still points at the right row if the model set changes.
end_labels <- do.call(
  rbind, 
  lapply(
    split(all_preds, all_preds$model), 
    function(d) {d[which.max(d$vviq), ]}
    )
  )
# end_kvamme <- pred_kvamme_typical[
#   which.max(pred_kvamme_typical$vviq), 
#   c("vviq", "estimate")]
# end_kvamme$model <- "Kvamme"
# end_labels <- rbind(end_labels, end_kvamme)

# Some end-of-line labels nudged slightly — at vviq=80 some curves sit close 
# enough that their text labels would overlap without these offsets.
end_labels["Linear",]$estimate <- end_labels["Linear",]$estimate + 0.3
end_labels["Segmented",]$estimate <- end_labels["Segmented",]$estimate + 0.5
end_labels["Floor-group",]$estimate <- end_labels["Floor-group",]$estimate - 0.4
end_labels["GAM",]$estimate <- end_labels["GAM",]$estimate - 1.4

# ------------------------------------------------------------------------------
# LOO comparison stats for the caption (hardcoded from the already-computed
# comparison table — CONSIDER replacing with a live read of
# comparison_all_models_tot.rds if you want this to stay auto-updating;
# left as hardcoded for now since the exact wording/rounding is easy to
# hand-tune here and the comparison is unlikely to change once locked).
# The real table:
#   comp_table <- readRDS(
#     "inst/results/comparison_all_models_tot.rds")
# ------------------------------------------------------------------------------
overlay_caption <- "elpd (relative to segmented): Floor-group \u22121.9 (SE 3.2); GAM \u22124.6 (SE 2.3); linear \u221222.4 (SE 7.5)"

# ------------------------------------------------------------------------------
# Assemble the plot
# ------------------------------------------------------------------------------
lw <- 0.4

p_overlay <-
  ggplot2::ggplot() +
  # Raw scatter, faint 
  ggplot2::geom_point(
    data = all_data,
    ggplot2::aes(x = vviq, y = tas),
    alpha = 0.1, size = 0.8, color = "grey60"
  ) +
  ggplot2::geom_line(
    data = pred_kvamme_aphant,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = lw - 0.1, linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = pred_kvamme_typical,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = lw - 0.1, linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = 32, color = "#377EB8", 
    linewidth = lw - 0.1, linetype = "dotted"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 33, y = 65,
    label = "Kvamme et al.'s\nfixed threshold",
    color = "#377EB8", size = 2, hjust = 0, vjust = 0
  ) +
  ggplot2::geom_line(
    data = all_preds,
    ggplot2::aes(x = vviq, y = estimate, color = model),
    linewidth = lw
  ) +
  ggplot2::geom_text(
    data = end_labels,
    ggplot2::aes(x = vviq, y = estimate, label = model, color = model),
    hjust = 0, nudge_x = 1, size = 1.85, fontface = "bold"
  ) +
  ggplot2::scale_color_manual(values = model_colors, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = seq(16, 80, 8),
    # limits/expansion tuned to fit end-of-line labels (esp. "Segmented",
    # the longest) within a single manuscript column without excess empty
    # margin — this replaced an earlier, wider version once single-column
    # width made that margin look empty; re-check if labels or column
    # width change.
    limits = c(15.5, 93),
    expand = ggplot2::expansion(c(0, 0))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(30, 75),
    expand = ggplot2::expansion(c(0.02, 0))
  ) +
  ggplot2::labs(
    x = "VVIQ score",
    y = "Total TAS score",
    caption = overlay_caption
  ) +
  theme_pdf(
    base_theme = ggplot2::theme_minimal,
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    # plot.caption = ggplot2::element_text(size = 5)
    plot.caption = ggplot2::element_blank()
  )

save_ggplot(
  "inst/visualisation/paper/fig_model_overlay.pdf", p_overlay,
  ncol = 1, height = 80)

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
# source("inst/visualisation/paper/plotting_segmented_knot.R")
# library(patchwork)
# 
# (p_overlay + 
#     ggplot2::ggtitle("Choosing among our candidate models") +
#     ggplot2::theme(plot.caption = ggplot2::element_text(hjust = -1))) + 
#   (p_segmented + ggplot2::ggtitle("Testing Kvamme et al.'s (2026) threshold")) + 
#   plot_layout(axis_titles = "collect")
# 
# save_ggplot(
#   "inst/visualisation/paper/fig_model_comparison.pdf",
#   ncol = 2, height = 110)
