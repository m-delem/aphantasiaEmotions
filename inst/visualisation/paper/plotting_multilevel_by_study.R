# ---------------------------------------------------------------------------- #
# Per-study slopes/intercepts — floor_group_additive_multilevel
# ---------------------------------------------------------------------------- #
# PURPOSE: directly answers a reviewer comment along the lines of "does the
# finding hold across studies" — visualises each study's own fitted line
# (intercept + vviq slope, from the multilevel model's per-study
# coefficients) against the pooled/population-level line, so a reader can
# see at a glance whether the five studies cluster around the same
# relationship or diverge meaningfully. This is reserved for
# floor_group_additive_multilevel specifically (the model carrying the
# manuscript's headline claim).
#
# The floor-group visual signature (violin + mean dot, muted/de-emphasised
# relative to the headline floor-group figure) is included so this figure
# is self-identifying as "the floor model, broken down by study" at a
# glance, without needing the reader to hunt in a caption.

devtools::load_all()

floor_group_additive_multilevel <-
  readRDS("inst/models/floor_group_additive_multilevel_tot.rds")
model_data <- floor_group_additive_multilevel$data  # already has complete_aphant, study, vviq, tas

# ---------------------------------------------------------------------------- #
# Per-study coefficients ----
# Per-study coefficients — brms::coef() (NOT fixef()) gives each study's
# own intercept + slope directly (population-level effect + that study's
# deviation, already summed) — no manual addition needed.
# ---------------------------------------------------------------------------- #
study_coefs <- coef(floor_group_additive_multilevel)$study
# study_coefs is a 3D array [study, stat, parameter] — reshape to a plain
# data frame for ggplot.
study_coefs_df <- data.frame(
  study     = dimnames(study_coefs)[[1]],
  intercept = study_coefs[, "Estimate", "Intercept"],
  slope     = study_coefs[, "Estimate", "vviq"]
)

# Pooled/population-level line, for reference
pooled_coefs <- brms::fixef(floor_group_additive_multilevel)
pooled_intercept <- pooled_coefs["Intercept", "Estimate"]
pooled_slope     <- pooled_coefs["vviq", "Estimate"]

# ---------------------------------------------------------------------------- #
# Per-study prediction lines ----
# Per-study prediction lines, EACH RESTRICTED TO THAT STUDY'S OWN OBSERVED
# VVIQ RANGE — same no-extrapolation principle already used for Kvamme's
# reference lines in plotting_segmented_knot.R. A study's line should only
# be shown where that study actually has data.
# ---------------------------------------------------------------------------- #
study_lines <- do.call(rbind, lapply(unique(model_data$study), function(s) {
  study_range <- range(model_data$vviq[model_data$study == s])
  coefs <- study_coefs_df[study_coefs_df$study == s, ]
  grid <- data.frame(
    vviq = seq(study_range[1], study_range[2], length.out = 100),
    study = s
  )
  grid$estimate <- coefs$intercept + coefs$slope * grid$vviq
  grid
}))

pooled_line <- data.frame(vviq = seq(16, 80, length.out = 100))
pooled_line$estimate <- pooled_intercept + pooled_slope * pooled_line$vviq

# ---------------------------------------------------------------------------- #
# Muted floor-group element ----
# Muted floor-group element — single pooled violin + mean point, NOT
# per-study (no complete_aphant | study random effect was fit, so a per-study
# breakdown here would visualise something the model doesn't actually estimate —
# reserved for the optional ridgeline block below instead, clearly marked as
# descriptive-only).
# ---------------------------------------------------------------------------- #
floor_raw <- model_data[model_data$complete_aphant == "floor", ]
dens <- stats::density(floor_raw$tas, from = 20, to = 100, n = 200)
dens_scaled <- dens$y / max(dens$y) * 2  # smaller/more muted than the headline figure's violin
violin_df <- data.frame(x = 16 - dens_scaled, xend = 16, y = dens$x)

floor_mean <- mean(floor_raw$tas)

# ---------------------------------------------------------------------------- #
# Assemble the plot ----
# ---------------------------------------------------------------------------- #
study_colors <- c(
  burns = "#E69F00", monzel = "#56B4E9", mas = "#009E73",
  ruby = "#D55E00", kvamme = "#CC79A7"
)
study_labels <-  c(
  burns = "Ale & B", monzel = "Monzel", mas = "Mas & L",
  ruby = "Ruby", kvamme = "Kvamme"
)

p_studies <-
  ggplot2::ggplot() +
  ggplot2::geom_point(
    data = model_data,
    ggplot2::aes(x = vviq, y = tas),
    alpha = 0.08, size = 0.8, color = "grey60"
  ) +
  # Muted floor-group violin (pooled, not per-study)
  ggplot2::geom_polygon(
    data = rbind(
      data.frame(x = violin_df$x, y = violin_df$y),
      data.frame(x = rev(violin_df$xend), y = rev(violin_df$y))
    ),
    ggplot2::aes(x = x, y = y),
    fill = "#C44E52", alpha = 0.35, color = "#8B3A3E", linewidth = 0.2
  ) +
  ggplot2::geom_point(
    data = data.frame(x = 16, y = floor_mean),
    ggplot2::aes(x = x, y = y),
    color = "#8B3A3E", size = 1.5
  ) +
  # Pooled/population-level line — bolder, black, the reference the
  # per-study lines should cluster around
  ggplot2::geom_line(
    data = pooled_line,
    ggplot2::aes(x = vviq, y = estimate),
    color = "black", linewidth = 0.5, linetype = "dashed"
  ) +
  # Per-study lines, each restricted to that study's own observed range
  ggplot2::geom_line(
    data = study_lines,
    ggplot2::aes(x = vviq, y = estimate, color = study),
    linewidth = 0.3, alpha = 0.85
  ) +
  ggplot2::scale_color_manual(
    values = study_colors, 
    labels = study_labels,
    name = "Study"
  ) +
  ggplot2::labs(
    x = "VVIQ score",
    y = "Total TAS score",
    # caption = "Floor-group N by study: Ale & Burns: 60; Monzel et al: 20\nMas et Luminet: 0; Ruby: 13; Kvamme et al: 54"
  ) +
  scale_x_vviq(breaks = seq(16, 80, by = 16)) +
  theme_pdf(
    base_theme = ggplot2::theme_minimal,
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    legend_relative = 0.9
  )

save_ggplot(
  "inst/visualisation/paper/fig_multilevel_by_study.pdf", p_studies, 
  ncol = 1, height = 100)

plot(p_studies)