# ==============================================================================
# Model fitting — segmented model with a FIXED knot from earth::earth()
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ==============================================================================
#
# This script FITS ONLY. All comparison, diagnostics, and PPCs live in
# model_diagnostics_and_comparison.R.
#
# Two parts:
#   A. Run earth::earth() on the data to find the optimal single-knot
#      breakpoint programmatically (rather than hardcoding a value found
#      interactively), so this script stays correct if the underlying data
#      ever changes.
#   B. Fit a Bayesian segmented regression in brms with that knot value
#      FIXED (not estimated) — a hinge parameterisation using explicit h_lo/
#      h_hi predictor columns, directly analogous to earth's own hinge basis.
#
# NOTE ON EXTRACTION: this reads the knot location from the earth object's
# `cuts` matrix, which stores the breakpoint used by each hinge term earth
# selected. CONFIRMED empirically (running mars$cuts[, "vviq"] directly):
# it returns (Intercept)=0, h(vviq-24)=24, h(24-vviq)=24 — i.e. one knot
# value (24) appearing once per hinge term sharing it, not two separate
# knots. The unique()-based extraction below already handles this correctly.

source("inst/scripts/00_model_comparison_setup.R")

# ------------------------------------------------------------------------------
# A. Find the knot with earth
# ------------------------------------------------------------------------------
mars <- earth::earth(tas ~ vviq, data = all_data)
# print(summary(mars))

# Extract the knot location programmatically from the fitted earth object.
# `mars$cuts` is a matrix (predictors x terms) of breakpoints used by each
# hinge basis function; for a single-predictor, single-knot model there
# should be exactly one non-zero cut value for `vviq`.
vviq_cuts <- mars$cuts[, "vviq"]
vviq_cuts_nonzero <- vviq_cuts[vviq_cuts != 0]

if (length(unique(vviq_cuts_nonzero)) != 1) {
  stop(
    "Expected exactly one distinct non-zero knot for vviq, found: ",
    paste(unique(vviq_cuts_nonzero), collapse = ", "),
    ". The model may have selected more than one knot, or none — inspect ",
    "summary(mars) directly before proceeding; the fixed-knot approach ",
    "below assumes a single breakpoint."
  )
}

knot <- unique(vviq_cuts_nonzero)
cat(sprintf("\nKnot extracted from earth::earth(): %.2f\n", knot))

# ------------------------------------------------------------------------------
# B. Fixed-knot segmented brms model
#
# Hinge predictors added to a copy of all_data (not modifying the package's
# canonical data object), mirroring earth's own h(knot-vviq) / h(vviq-knot)
# basis exactly:
#   h_lo = max(knot - vviq, 0)   -> active (and increasing) for vviq < knot
#   h_hi = max(vviq - knot, 0)   -> active (and increasing) for vviq > knot
# ------------------------------------------------------------------------------
model_data <- all_data
model_data$h_lo <- pmax(knot - model_data$vviq, 0)
model_data$h_hi <- pmax(model_data$vviq - knot, 0)

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
    file = paste0(COMPARISON_MODEL_DIR, "segmented_fixed_knot_tot.rds")
  )

# ------------------------------------------------------------------------------
# NOTE on sign convention (relevant when interpreting output later):
# h_lo = knot - vviq, so an INCREASE in vviq DECREASES h_lo. This means the
# sign of the h_lo coefficient is flipped relative to what you'd read as
# "the slope of tas on vviq below the knot" — a NEGATIVE h_lo coefficient
# corresponds to a POSITIVE slope of tas on vviq in that region, and vice
# versa. Double-check this against summary(segmented_fixed) before writing
# anything up; getting this backwards was flagged as a risk earlier and is
# worth actively verifying, not assuming.
# ------------------------------------------------------------------------------

cat("---------------------------------------------------------------------------------\n")
cat("Script 02 done: segmented_fixed fit and saved to", COMPARISON_MODEL_DIR, "\n")
cat("Run model_diagnostics_and_comparison.R for checks and comparison tables.\n")
cat("---------------------------------------------------------------------------------\n")
