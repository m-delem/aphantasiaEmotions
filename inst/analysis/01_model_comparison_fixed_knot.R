# ==============================================================================
# Model comparison — fixed-knot segmented model vs. existing candidates
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ==============================================================================
#
# Candidates:
#   1. Linear:      tas ~ vviq                   (NEW — not in current vignette)
#   2. Categorical: tas ~ vviq_group_4           (existing, from vignette)
#   3. GAM:         tas ~ s(vviq)                (existing, from vignette)
#   4. Segmented:   tas ~ hinge terms at VVIQ=24 (NEW — fixed knot from MARS)
#
# The knot at VVIQ = 24 comes from the earth::earth() run:
#   h(24-vviq)  coefficient -0.941  (steep segment, VVIQ < 24)
#   h(vviq-24)  coefficient -0.294  (shallower segment, VVIQ > 24)
# This script treats 24 as FIXED and known. See script 02 for the version
# where the knot location is itself estimated.
#
# Run 00_model_comparison_setup.R first (or source() it below).

source("inst/analysis/00_model_comparison_setup.R")  # adjust path as needed

# ------------------------------------------------------------------------------
# Prepare hinge predictors for the fixed-knot segmented model.
#
# We add explicit hinge columns to a COPY of all_data rather than relying on
# in-formula I()/pmax() expressions, so the same columns can be reused for
# plotting/prediction later without re-deriving them.
#   h_lo = max(24 - vviq, 0)   -> active (and increasing) for vviq < 24
#   h_hi = max(vviq - 24, 0)   -> active (and increasing) for vviq > 24
# This mirrors earth's own h(24-vviq) / h(vviq-24) hinge basis exactly.
# ------------------------------------------------------------------------------
knot <- 24

model_data <- all_data
model_data$h_lo <- pmax(knot - model_data$vviq, 0)
model_data$h_hi <- pmax(model_data$vviq - knot, 0)

# ------------------------------------------------------------------------------
# 1. Linear model (new — baseline continuous comparison point)
# ------------------------------------------------------------------------------
lm_linear <-
  fit_brms_model(
    formula = tas ~ vviq,
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "lm_linear_tot.rds")
  )

# ------------------------------------------------------------------------------
# 2. Categorical model (existing formula, re-fit here at the comparison
#    iteration budget so WAIC/LOO are computed on a consistent footing across
#    all four candidates — the vignette's own fit used a different, higher
#    iteration count, which is fine for reporting but not for a fair comparison)
# ------------------------------------------------------------------------------
lm_categorical <-
  fit_brms_model(
    formula = tas ~ vviq_group_4,
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "lm_categorical_tot.rds")
  )

# ------------------------------------------------------------------------------
# 3. GAM (existing formula, re-fit at comparison budget — same rationale as above)
# ------------------------------------------------------------------------------
gam_tot <-
  fit_brms_model(
    formula = tas ~ s(vviq),
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.99,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "gam_tot.rds")
  )

# ------------------------------------------------------------------------------
# 4. Segmented model, fixed knot at VVIQ = 24
#
# tas ~ h_lo + h_hi  gives two slope coefficients, directly analogous to
# earth's two hinge coefficients, but now with full posterior uncertainty
# instead of a single point estimate.
# ------------------------------------------------------------------------------
segmented_fixed <-
  fit_brms_model(
    formula = tas ~ h_lo + h_hi,
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = ADAPT_DELTA_DEFAULT,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "segmented_fixed_knot24_tot.rds")
  )

# ------------------------------------------------------------------------------
# Diagnostics — CHECK THESE BEFORE TRUSTING THE COMPARISON BELOW
# ------------------------------------------------------------------------------
cat("\n--- Rhat / ESS summary checks ---\n")
for (nm in c("lm_linear", "lm_categorical", "gam_tot", "segmented_fixed")) {
  m <- get(nm)
  s <- brms::rhat(m)
  cat(sprintf(
    "%-16s max Rhat = %.4f | min bulk ESS ~ %.0f\n",
    nm, max(s, na.rm = TRUE),
    min(brms::neff_ratio(m) * brms::ndraws(m), na.rm = TRUE)
  ))
}
cat("If any max Rhat > 1.01 or min ESS < 400: bump ITERATIONS_COMPARISON for\n")
cat("that specific model and re-fit before trusting its LOO/WAIC value.\n\n")

# Posterior predictive checks (visual, one per model — inspect manually)
# performance::check_predictions(lm_linear)
# performance::check_predictions(lm_categorical)
# performance::check_predictions(gam_tot)
# performance::check_predictions(segmented_fixed)

# ------------------------------------------------------------------------------
# WAIC/LOO comparison
# ------------------------------------------------------------------------------
comparison_fixed_knot <- compare_models_loo(
  list(
    linear      = lm_linear,
    categorical = lm_categorical,
    gam         = gam_tot,
    segmented   = segmented_fixed
  )
)

print(comparison_fixed_knot)

# Save the comparison table for later reference (manuscript table, EOR page)
saveRDS(
  comparison_fixed_knot, 
  paste0(COMPARISON_MODEL_DIR, "comparison_fixed_knot_tot.rds"))
write.csv(
  comparison_fixed_knot,
  paste0(COMPARISON_MODEL_DIR, "comparison_fixed_knot_tot.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# NOTE on the segmented model's coefficients vs. earth's:
#
# Unlike earth (which reports coefficients relative to its internal hinge
# parameterisation), this brms model's h_lo/h_hi coefficients can be read
# directly as slopes:
#   - coefficient on h_hi = slope of tas on vviq FOR vviq > 24
#   - (coefficient on h_lo) with a sign flip = slope of tas on vviq for
#     vviq < 24 (since h_lo = 24 - vviq for vviq < 24, an increase in vviq
#     DECREASES h_lo, so the sign relationship is inverted relative to h_hi)
# Double check this against summary(segmented_fixed) and against earth's
# reported coefficients before writing up — get the sign convention right
# before this goes anywhere near the manuscript.
# ------------------------------------------------------------------------------

