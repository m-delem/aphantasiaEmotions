# ==============================================================================
# Family choice diagnostics — justifying gaussian() for TAS-20 total
# ==============================================================================
#
# Purpose: gather concrete evidence (not just "it's standard practice") for
# why gaussian() was used, to be written up as an EOR paragraph later. This
# script does NOT fit any new models — it reuses the fits from
# 01_model_comparison_fixed_knot.R.
#
# Rationale for gaussian() a priori: TAS-20 total is a sum of 20 five-point
# items (range 20-100 in principle), so it's bounded and technically
# discrete, but with enough range/granularity that a continuous
# approximation is standard in this literature. The checks below aren't
# meant to "prove" gaussian() is correct — no check can do that — they're
# meant to surface any obvious problem (skew, heteroscedasticity, boundary
# effects) that would call for a bounded/skewed alternative (e.g.
# ordered-beta, skew-normal) instead.
#
# Personal read from performance::check_predictions() during fitting: TAS
# distribution already looks close to normal, PPC results satisfying. This
# script exists to have something concrete and citable to point to for the
# EOR paragraph, not because there's a live suspicion something is wrong.

library(brms)
library(performance)

# Reload fitted models if not already in the environment
model_dir <- "inst/analysis/models_comparison/"
lm_linear       <- readRDS(paste0(model_dir, "lm_linear_tot.rds"))
lm_categorical  <- readRDS(paste0(model_dir, "lm_categorical_tot.rds"))
gam_tot         <- readRDS(paste0(model_dir, "gam_tot.rds"))
segmented_fixed <- readRDS(paste0(model_dir, "segmented_fixed_knot24_tot.rds"))

models <- list(
  linear      = lm_linear,
  categorical = lm_categorical,
  gam         = gam_tot,
  segmented   = segmented_fixed
)

# ------------------------------------------------------------------------------
# 1. Raw outcome shape — is TAS total itself close to normal, independent of
#    any model? Quick, model-free sanity check.
# ------------------------------------------------------------------------------
cat("--- Raw TAS total: skewness and range ---\n")
tas_vals <- lm_linear$data$tas  # same outcome vector for all four models
cat(sprintf("Range: [%.1f, %.1f]\n", min(tas_vals), max(tas_vals)))
cat(sprintf("Skewness: %.3f (near 0 = symmetric)\n",
    mean((tas_vals - mean(tas_vals))^3) / sd(tas_vals)^3))
cat(sprintf("Any values at theoretical boundaries (20 or 100)? %s\n",
    any(tas_vals <= 20 | tas_vals >= 100)))

# ------------------------------------------------------------------------------
# 2. Residual diagnostics per model — standardised residuals should show no
#    strong skew and no obvious funnel (heteroscedasticity) against fitted
#    values.
# ------------------------------------------------------------------------------
cat("\n--- Residual skewness per model (near 0 = symmetric) ---\n")
for (nm in names(models)) {
  m <- models[[nm]]
  resid_vals <- residuals(m)[, "Estimate"]
  skew <- mean((resid_vals - mean(resid_vals))^3) / sd(resid_vals)^3
  cat(sprintf("%-14s skewness = %.3f\n", nm, skew))
}

# ------------------------------------------------------------------------------
# 3. Posterior predictive checks — visual, saved as objects for reuse in the
#    EOR page later rather than only viewed interactively now.
# ------------------------------------------------------------------------------
pp_checks <- lapply(
  models, 
  function(m) performance::check_predictions(m, draw_ids = 1:12))

# To view: plot(pp_checks$linear), plot(pp_checks$gam), etc.
# Save for later EOR use:
saveRDS(pp_checks, paste0(model_dir, "family_diagnostics_pp_checks.rds"))

# ------------------------------------------------------------------------------
# 4. Heteroscedasticity spot-check: correlation between |residuals| and
#    fitted values. A strong positive correlation would suggest variance
#    grows with the mean (common reason to reach for a non-gaussian family).
# ------------------------------------------------------------------------------
cat("\n--- |Residual| vs fitted value correlation (near 0 = homoscedastic) ---\n")
for (nm in names(models)) {
  m <- models[[nm]]
  fitted_vals <- fitted(m)[, "Estimate"]
  resid_vals  <- residuals(m)[, "Estimate"]
  r <- cor(abs(resid_vals), fitted_vals)
  cat(sprintf("%-14s r = %.3f\n", nm, r))
}

# ------------------------------------------------------------------------------
# Summary for the EOR paragraph (fill in after running):
#   - Raw TAS skewness: ___
#   - No values at theoretical boundaries: ___
#   - Residual skewness across models: ___ (all close to 0? any outliers?)
#   - Heteroscedasticity correlations: ___ (all near 0? any concerning?)
#   - PPC plots: consistent with what Maël already observed during fitting?
# If all of the above look clean, the EOR paragraph can state plainly that
# gaussian() was checked, not just assumed, and name the specific
# diagnostics used — without needing to fit an alternative family for
# comparison, since nothing here would have motivated one.
# ------------------------------------------------------------------------------
