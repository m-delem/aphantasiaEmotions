# ---------------------------------------------------------------------------- #
# Model fitting — segmented model with an ESTIMATED (not fixed) knot location
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ---------------------------------------------------------------------------- #
#
# This script FITS ONLY. All comparison, diagnostics, and PPCs live in
# model_diagnostics_and_comparison.R.
#
# This model lets brms estimate the breakpoint as a free parameter, with a
# prior centred near the earth-derived knot (script 02), rather than fixing
# it. This gives a full posterior — and a credible interval — on WHERE the
# regime change happens.
#
# VALIDATED: this formula/init/fit combination was already run successfully
# (full 6-chain run, all Rhat ~1.00-1.002, min bulk ESS ~2088, no reported
# divergence/treedepth issues). Two earlier bugs are already fixed in this
# version:
#   - fmin()/fmax() (valid in Stan, NOT in R) replaced with step(), which
#     brms handles correctly in both contexts — confirmed via
#     brms:::step(c(-1,0,1)) returning 0 1 1.
#   - Explicit `init` values added: the sampler initially wandered to a
#     nonsensical knot location (k = -3.39, outside VVIQ's 16-80 range) under
#     brms's naive default init; explicit inits centred on sensible starting
#     guesses fixed this completely.
#
# Result obtained previously (for reference — re-derive from THIS run's
# fitted object once it completes, don't just reuse these numbers):
#   knot: 19.50 [17.74, 24.09]
#   below-knot slope (b1): 2.46 [0.90, 4.76]
#   above-knot slope (b1+b2): -0.28 [-0.33, -0.23]

source("inst/scripts/00_model_comparison_setup.R")

# ---------------------------------------------------------------------------- #
# Nonlinear formula design ----
#
#   tas = a + b1 * vviq + b2 * (vviq - k) * step(vviq - k)
#
# Reading the coefficients under this parameterisation:
#   - b1 = slope of tas on vviq BELOW the knot (step() = 0 there)
#   - b1 + b2 = slope of tas on vviq ABOVE the knot (step() = 1 there)
#   - b2 = the CHANGE in slope at the knot (not a segment slope on its own)
# ---------------------------------------------------------------------------- #
segmented_formula <- brms::bf(
  tas ~ a + b1 * vviq + b2 * (vviq - k) * step(vviq - k),
  a  ~ 1,
  b1 ~ 1,
  b2 ~ 1,
  k  ~ 1,
  nl = TRUE
)

# ---------------------------------------------------------------------------- #
# Priors for the nonlinear parameters ----
#   a, b1, b2: normal(0, 20), matching the package's general convention.
#     (b2 is a slope-DIFFERENCE, not a raw slope)
#   k (knot location): normal(24, 10), centred near the earth-derived value
#     from script 02, wide enough for the data to override it.
# ---------------------------------------------------------------------------- #
segmented_priors <- c(
  brms::prior(normal(0, 20), nlpar = "a"),
  brms::prior(normal(0, 20), nlpar = "b1"),
  brms::prior(normal(0, 20), nlpar = "b2"),
  brms::prior(normal(24, 10), nlpar = "k")
)

# ---------------------------------------------------------------------------- #
# Sanity check ----
# SANITY CHECK — run this FIRST if re-verifying after any change. Does not
# fit anything, just generates Stan code.
# ---------------------------------------------------------------------------- #
# stan_check <- brms::make_stancode(
#   segmented_formula,
#   data = all_data,
#   prior = segmented_priors
# )
# cat(stan_check)

# ---------------------------------------------------------------------------- #
# Initial values ----
# Explicit initial values for the nonlinear parameters — REQUIRED. Without
# this, the sampler previously found a nonsensical knot location (see note
# above). Centred on sensible starting guesses (matching the priors' own
# centres, not fitted values).
# ---------------------------------------------------------------------------- #
segmented_inits <- function() {
  list(
    b_a  = array(mean(all_data$tas)), # intercept ~ near sample mean of TAS
    b_b1 = array(0),                  # slope ~ start neutral
    b_b2 = array(0),                  # slope-change ~ start neutral
    b_k  = array(24)                  # knot ~ prior centre, matches earth's
  )
}

# ---------------------------------------------------------------------------- #
# Full fit ----
# adapt_delta raised to 0.99 and max_treedepth to 15 as precautions for this
# nonlinear/hinge model's potentially trickier posterior geometry.
# ---------------------------------------------------------------------------- #
segmented_estimated <-
  fit_brms_model(
    formula = segmented_formula,
    data = all_data,
    prior = segmented_priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.99,
    max_treedepth = 15,
    init = segmented_inits,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "segmented_estimated_knot_tot.rds")
  )

cat("---------------------------------------------------------------------------------\n")
cat("Script 03 done: segmented_estimated fit and saved to", COMPARISON_MODEL_DIR, "\n")
cat("Run model_diagnostics_and_comparison.R for checks, derived slopes, and comparison\ntables.\n")
cat("---------------------------------------------------------------------------------\n")
