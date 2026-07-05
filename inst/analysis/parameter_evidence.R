# ==============================================================================
# Parameter evidence — pd/ROPE for floor_group_additive and segmented_estimated
# ==============================================================================
#
# Extends the pd/ROPE evidentiary treatment already used for the categorical
# and GAM models (see main vignette) to the two new models of interest.
#
# METHOD NOTES (from probe_modelbased_compatibility.R):
#   - describe_posterior() works cleanly and gives a complete pd+CI+ROPE
#     summary in one call — confirmed on floor_group_additive.
#   - p_direction()/describe_posterior() had naming/formatting problems when
#     given segmented_estimated's raw Stan-level nonlinear parameter names
#     (b_k_Intercept mislabelled as "(Intercept)"; b_b1_Intercept errored
#     outright). Root cause not fully isolated — possibly expects brms's
#     short-form nlpar names rather than full Stan parameter strings.
#   - The MANUAL approach (extract posterior draws directly via
#     as_draws_df(), run bayestestR::p_direction()/rope() on the resulting
#     plain numeric vector) is CONFIRMED to work cleanly regardless (probe
#     Test 4) — used here for all segmented_estimated parameters rather than
#     fighting the naming issue further.
#   - ROPE range: TWO conventions used, not one. The floor-group CONTRAST
#     (complete_aphant) uses bayestestR::rope_range() (0.1 x SD of TAS),
#     matching the existing categorical/GAM convention — appropriate since
#     it's a discrete group shift, directly comparable to a mean
#     difference. SLOPES (vviq, below/above-knot slopes) use a DIFFERENT,
#     Cohen-motivated ROPE (|0.2| in standardized units, rescaled to raw
#     vviq/tas units via SD(tas)/SD(vviq)) — a raw slope isn't comparable
#     to a raw-response-scale ROPE the way a group contrast is, since
#     "one unit of vviq" isn't a standardized step. Confirmed against
#     bayestestR's own docs, which use a similarly different convention
#     (+/-0.05) for correlations, another standardized-effect-type quantity.

library(brms)
library(bayestestR)

floor_group_additive <- readRDS("inst/analysis/models_comparison/floor_group_additive_tot.rds")
segmented_estimated  <- readRDS("inst/analysis/models_comparison/segmented_estimated_knot_tot.rds")

# ------------------------------------------------------------------------------
# ROPE ranges — TWO DIFFERENT CONVENTIONS NEEDED, NOT ONE.
#
# rope_range_tas (0.1 x SD(tas)) is appropriate for the floor-group
# CONTRAST (complete_aphant), which is a discrete group shift directly
# comparable to a mean difference — this matches the existing convention
# already used for the categorical/GAM analyses.
#
# It is NOT appropriate for SLOPES (vviq's coefficient, or the below/
# above-knot slopes in segmented_estimated). A slope is "per one unit of
# vviq", and vviq's scale (16-80) is arbitrary relative to TAS's scale —
# comparing a raw slope to 0.1 x SD(tas) implicitly assumes "one unit of
# vviq" is a standardized step, which it isn't. Confirmed via bayestestR's
# own documentation: for CORRELATIONS (another standardized-effect-type
# quantity), bayestestR uses a different default (+/-0.05, "half a
# negligible correlation per Cohen"), not the raw-response-scale
# rope_range() formula — slopes need the same kind of standardized
# treatment, not the group-contrast formula.
#
# Fix: rescale Cohen's standardized "small effect" convention (Maël's
# instinct: |slope| > 0.2 in STANDARDIZED units is already noticeable) into
# the RAW units these slopes are actually expressed in, via
# SD(tas)/SD(vviq). This correctly answers "what raw-scale slope
# CORRESPONDS TO a standardized slope of 0.2", rather than reusing a
# formula meant for group contrasts.
# ------------------------------------------------------------------------------
rope_range_tas <- bayestestR::rope_range(floor_group_additive)
cat(sprintf("ROPE range for the FLOOR-GROUP CONTRAST (0.1 x SD of TAS): [%.3f, %.3f]\n",
            rope_range_tas[1], rope_range_tas[2]))

sd_tas  <- stats::sd(floor_group_additive$data$tas)
sd_vviq <- stats::sd(floor_group_additive$data$vviq)

rope_range_slope <- 0.2 * (sd_tas / sd_vviq)
cat(sprintf(
  "ROPE range for SLOPES (Cohen |0.2| standardized, rescaled to raw vviq/tas units): [%.4f, %.4f]\n\n",
  -rope_range_slope, rope_range_slope
))

# ==============================================================================
# 1. floor_group_additive — the headline floor effect
# ==============================================================================
cat("=== floor_group_additive: floor-group effect ===\n\n")

floor_effect_summary <- bayestestR::describe_posterior(
  floor_group_additive,
  parameters = "complete_aphant",
  rope_range = rope_range_tas
)
print(floor_effect_summary)

# Also report the main vviq slope (above-floor group) for completeness —
# this is the "everyone else follows one continuous relationship" half of
# the claim. Uses rope_range_slope (Cohen-based, rescaled), NOT
# rope_range_tas — this is a slope, not a group contrast.
cat("\n=== floor_group_additive: above-floor vviq slope ===\n\n")
vviq_slope_summary <- bayestestR::describe_posterior(
  floor_group_additive,
  parameters = "vviq",
  rope_range = c(-rope_range_slope, rope_range_slope)
)
print(vviq_slope_summary)

# ==============================================================================
# 2. segmented_estimated — knot location, below-knot slope, above-knot slope
#
# All computed via MANUAL posterior draw extraction (confirmed-safe path),
# not describe_posterior() directly on the model object.
# ==============================================================================
cat("\n=== segmented_estimated: knot location, below/above-knot slopes ===\n\n")

draws <- brms::as_draws_df(
  segmented_estimated,
  variable = c("b_k_Intercept", "b_b1_Intercept", "b_b2_Intercept")
)

below_knot_slope <- draws$b_b1_Intercept
above_knot_slope <- draws$b_b1_Intercept + draws$b_b2_Intercept
knot_location    <- draws$b_k_Intercept

report_manual_evidence <- function(draws_vector, label, rope_range) {
  pd_val   <- bayestestR::p_direction(draws_vector)
  rope_val <- bayestestR::rope(draws_vector, range = rope_range)
  ci       <- stats::quantile(draws_vector, probs = c(0.025, 0.5, 0.975))
  cat(sprintf(
    "%-20s Median = %.3f  [%.3f, %.3f]  pd = %s  %% in ROPE = %s\n",
    label, ci[2], ci[1], ci[3],
    format(pd_val$pd * 100, digits = 4),
    format(rope_val$ROPE_Percentage * 100, digits = 4)
  ))
}

# NOTE: the knot location's ROPE range doesn't make sense on the same
# TAS-based rope_range_tas scale (VVIQ and TAS are different scales
# entirely). Reporting pd/CI for the knot, but NOT a ROPE percentage against
# rope_range_tas — that would be a category error. If a ROPE-style claim
# about the knot is wanted (e.g. "is the knot credibly below 32"), that's a
# DIFFERENT, deliberate comparison — see section 3 below — not a generic
# ROPE-around-zero, which is meaningless for a location parameter on the
# VVIQ scale.
cat("Below-knot slope (b1):\n")
report_manual_evidence(below_knot_slope, "below_knot_slope", c(-rope_range_slope, rope_range_slope))

cat("\nAbove-knot slope (b1+b2):\n")
report_manual_evidence(above_knot_slope, "above_knot_slope", c(-rope_range_slope, rope_range_slope))

cat("\nKnot location (VVIQ scale — pd/CI only, no TAS-based ROPE):\n")
knot_pd <- bayestestR::p_direction(knot_location)
knot_ci <- stats::quantile(knot_location, probs = c(0.025, 0.5, 0.975))
cat(sprintf("knot_location        Median = %.2f  [%.2f, %.2f]  pd = %s\n",
            knot_ci[2], knot_ci[1], knot_ci[3], format(knot_pd$pd * 100, digits = 4)))

# ==============================================================================
# 3. Knot vs. reference thresholds — a deliberate, substantive comparison
#
# Rather than a generic ROPE-around-zero (meaningless for a location
# parameter), this asks a specific, citable question: is the ESTIMATED knot
# credibly BELOW Kvamme et al.'s manually-chosen threshold (~32-33)? This
# operationalises the "hints at redefining the aphantasia/hypophantasia
# boundary" observation with actual evidence, rather than an eyeballed CI
# comparison.
#
# KVAMME_THRESHOLD below is Maël's recollection of Kvamme et al.'s reported
# cutoff — CONFIRM this value against the actual paper before using it in
# any write-up; not independently verified here.
# ==============================================================================
cat("\n=== Knot location vs. Kvamme et al.'s manual threshold ===\n\n")

KVAMME_THRESHOLD <- 32  # CONFIRM against Kvamme et al. before using in write-up

prop_below_kvamme <- mean(knot_location < KVAMME_THRESHOLD)
cat(sprintf(
  "Proportion of posterior draws where the estimated knot < %.0f (Kvamme's threshold): %.1f%%\n",
  KVAMME_THRESHOLD, prop_below_kvamme * 100
))
cat("This is a direct, substantive claim about boundary placement — consider\n")
cat("reporting this alongside the knot's own CI in the manuscript/EOR.\n")