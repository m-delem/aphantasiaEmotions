# ==============================================================================
# Model fitting — floor-group additive model
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ==============================================================================
#
# This script FITS ONLY. All comparison, diagnostics, and PPCs live in
# model_diagnostics_and_comparison.R.
#
# RATIONALE: the pooled VVIQ distribution shows a sharp, isolated spike at
# VVIQ = 16 (the scale's hard floor) — visually distinct from the rest of
# the distribution, which is more continuous (if irregular) from ~20 to 80.
# This is plausibly a genuine floor/boundary effect (many "complete
# aphantasia" respondents piling up at the minimum possible score) rather
# than evidence the whole VVIQ-TAS relationship is smoothly continuous.
#
# Rather than fitting a full mixture-regression model (flexmix-style — a
# heavier, qualitatively different claim about latent subpopulations,
# parked as a documented future direction), this model takes a lighter,
# well-justified intermediate step: an explicit binary indicator for the
# floor group, letting its mean TAS level differ from the extrapolation of
# the continuous VVIQ-TAS relationship fit on everyone else.
#
# IMPORTANT — SPECIFICATION HISTORY: an earlier version of this script used
# an INTERACTION model (tas ~ vviq * complete_aphant), on the reasoning that
# the floor group might have a different SLOPE, not just a different
# intercept. Maël caught the flaw in this before running it: since every
# floor-group observation has vviq = 16 (zero within-group variance), there
# is no data to estimate a floor-group-specific slope from — the
# interaction term cannot be identified from floor-group data alone. This
# was confirmed empirically: the fitted interaction model showed
# `vviq:complete_aphantfloor` = -0.57 [-3.06, 1.89], an enormous,
# uninformative interval, and `complete_aphantfloor` (confounded with it)
# came out as 0.37 [-39.09, 39.90] — both consistent with a non-identified
# parameter, not "no effect." The additive model below is the correctly
# specified version: it asks the answerable question (does the floor
# group's MEAN differ from where the continuous relationship would predict
# it, at VVIQ=16), not the unanswerable one (does its slope differ).

source("inst/analysis/00_model_comparison_setup.R")

# ------------------------------------------------------------------------------
# Floor-group indicator, derived from the existing vviq_group_4 factor
# ------------------------------------------------------------------------------
model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")  # above_floor as reference level
)

cat("Floor group sizes:\n")
print(table(model_data$complete_aphant))

# ------------------------------------------------------------------------------
# Additive model: floor group gets its own intercept-shift only. This is
# the estimable quantity given the floor group has zero within-group VVIQ
# variance — see specification history note above.
# ------------------------------------------------------------------------------
floor_group_additive <-
  fit_brms_model(
    formula = tas ~ vviq + complete_aphant,
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_tot.rds")
  )

# ------------------------------------------------------------------------------
# NOTE on reading coefficients: with above_floor as the reference level,
#   - `vviq` coefficient = slope of tas on vviq for the ABOVE-FLOOR group
#     (the only group with within-group VVIQ variance to estimate a slope
#     from)
#   - `complete_aphantfloor` coefficient = how far the floor group's MEAN
#     TAS sits above (positive) or below (negative) what the continuous
#     vviq-tas relationship, extrapolated down to vviq=16, would predict.
#     This is the coefficient the floor-effect hypothesis actually rests on.
#
# Result obtained previously (for reference — re-derive from THIS run's
# fitted object once it completes):
#   complete_aphantfloor: -8.75 [-11.38, -6.11] — floor group's mean TAS
#   sits about 8.75 points BELOW the continuous relationship's extrapolated
#   prediction at vviq=16, a tight, clearly non-zero effect (unlike the
#   non-identified interaction term it replaces).
# ------------------------------------------------------------------------------

cat("Script 04 done: floor_group_additive fit and saved to", COMPARISON_MODEL_DIR, "\n")
cat("Run model_diagnostics_and_comparison.R for checks and comparison tables.\n")