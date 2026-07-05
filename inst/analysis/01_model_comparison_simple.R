# ==============================================================================
# Model fitting — simple candidates (linear, categorical, GAM)
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ==============================================================================
#
# This script FITS ONLY. All comparison, diagnostics, and PPCs live in
# model_diagnostics_and_comparison.R, which loads every fitted model from
# COMPARISON_MODEL_DIR and works on them together.
#
# Candidates fit here:
#   1. Linear:      tas ~ vviq
#   2. Categorical: tas ~ vviq_group_4  (existing 4-group VVIQ factor)
#   3. GAM:         tas ~ s(vviq)
#
# The segmented (fixed- and estimated-knot) models live in scripts 02 and 03;
# the floor-group interaction model lives in script 04.

source("inst/analysis/00_model_comparison_setup.R")

# ------------------------------------------------------------------------------
# 1. Linear model
# ------------------------------------------------------------------------------
lm_linear <-
  fit_brms_model(
    formula = tas ~ vviq,
    data = all_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "lm_linear_tot.rds")
  )

# ------------------------------------------------------------------------------
# 2. Categorical model (4-group VVIQ factor)
# ------------------------------------------------------------------------------
lm_categorical <-
  fit_brms_model(
    formula = tas ~ vviq_group_4,
    data = all_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "lm_categorical_tot.rds")
  )
lm_categorical_2 <-
  fit_brms_model(
    formula = tas ~ vviq_group_2,
    data = all_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "lm_categorical_2_group_tot.rds")
  )

# Note: gam_tot needed adapt_delta = 0.99 in practice (13 divergent
# transitions at the package default of 0.95). Set directly here rather
# than relying on ADAPT_DELTA_DEFAULT, since this is now a KNOWN
# requirement for this specific model, not a speculative precaution.

# ------------------------------------------------------------------------------
# 3. GAM
# ------------------------------------------------------------------------------
gam_tot <-
  fit_brms_model(
    formula = tas ~ s(vviq),
    data = all_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.99,  # known requirement — see note above
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "gam_tot.rds")
  )

cat("Script 01 done: lm_linear, lm_categorical, gam_tot fit and saved to",
    COMPARISON_MODEL_DIR, "\n")
cat("Run model_diagnostics_and_comparison.R for checks and comparison tables.\n")
