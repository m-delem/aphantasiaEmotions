# ==============================================================================
# Segmented model with an ESTIMATED (not fixed) knot location
# Outcome: total TAS-20 score. Single-level (no study random effects yet).
# ==============================================================================
#
# This is the more ambitious version of the segmented model: instead of
# hard-coding the breakpoint at VVIQ = 24 (from earth), we let brms estimate
# the breakpoint as a free parameter, with a prior centred near 24. This
# gives a full posterior — and a credible interval — on WHERE the regime
# change happens, which is a substantively more interesting and more
# honestly Bayesian claim than a fixed threshold (ours or Kvamme's).
#
# IMPORTANT: run the sanity check block below FIRST, using
# brms::make_stancode(), before attempting a full sampling run. This only
# generates Stan code and checks that the formula is well-formed — it does
# NOT fit anything, so it's fast, and it will catch formula/syntax errors
# before you spend real sampling time on a broken specification. This
# script was written without the ability to test-run it locally, so treat
# the formula below as a well-reasoned draft, not a guaranteed-correct one.
#
# Run 00_model_comparison_setup.R first.

source("inst/analysis/00_model_comparison_setup.R")  # adjust path as needed

# ------------------------------------------------------------------------------
# Nonlinear formula design
#
# We parameterise the segmented relationship directly in terms of a free
# knot parameter `k`, an intercept `a`, a below-knot slope `b1`, and a
# slope-change-at-the-knot term `b2` (see the revised formula design note
# below for how this parameterisation reads):
#
#   tas = a + b1 * vviq + b2 * (vviq - k) * step(vviq - k)
#
# This is algebraically equivalent to the fixed-knot hinge parameterisation
# in script 01, but written so that `k` is itself a model parameter brms can
# put a prior on and estimate, rather than a value baked into the data
# beforehand.
#
# brms nonlinear syntax requires:
#   - the outer bf() call naming the nonlinear formula
#   - one nlf() or lf() line per nonlinear parameter (a, b1, b2, k)
#   - priors set explicitly on each nonlinear parameter via class = "b" with
#     nlpar = "..." (brms does NOT reuse the flat prior convention for
#     nonlinear parameters the way it does for standard fixed effects, so
#     the `priors` object from the setup script is NOT sufficient here —
#     new, explicit priors are set below)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Nonlinear formula design (REVISED)
#
# The first version of this script used fmin()/fmax(), which are valid Stan
# functions but do NOT exist in R. That let the model COMPILE and SAMPLE
# fine, but broke afterward: brms needs to re-evaluate the nonlinear formula
# in R for prediction, fitted(), and loo() — and R has no fmin/fmax, so those
# post-processing steps failed with "impossible de trouver la fonction fmin".
#
# Fixed by reparameterising with step(), which brms explicitly supports in
# nonlinear formulas and which is valid in BOTH Stan and R:
#
#   tas = a + b1 * vviq + b2 * (vviq - k) * step(vviq - k)
#
# Reading the coefficients under this parameterisation (different from the
# fmin/fmax version!):
#   - b1 = slope of tas on vviq BELOW the knot (step() = 0 there)
#   - b1 + b2 = slope of tas on vviq ABOVE the knot (step() = 1 there)
#   - b2 = the CHANGE in slope at the knot (not a segment slope on its own)
# This is a different (but equivalent in substance) parameterisation from
# the earlier fmin/fmax draft — don't directly compare b1/b2 here to
# b1/b2 in older output; recompute the "above-knot" slope as b1 + b2.
# ------------------------------------------------------------------------------

segmented_formula <- brms::bf(
  tas ~ a + b1 * vviq + b2 * (vviq - k) * step(vviq - k),
  a  ~ 1,
  b1 ~ 1,
  b2 ~ 1,
  k  ~ 1,
  nl = TRUE
)

# ------------------------------------------------------------------------------
# Priors for the nonlinear parameters (REVISED for the step() parameterisation).
#
#   a  (intercept): weakly informative, matching the existing normal(0, 20)
#      convention used elsewhere in this package for effects on the TAS scale.
#   b1 (below-knot slope): same normal(0, 20) convention.
#   b2 (CHANGE in slope at the knot, not a segment slope on its own): kept at
#      normal(0, 20) for consistency with the rest of the package's prior
#      philosophy, though it's worth flagging that b2 is a *difference*
#      between two slopes, so a tighter prior could arguably be justified
#      (a slope-difference is intrinsically a smaller quantity than a raw
#      slope). Not changing it now to avoid introducing a second prior
#      philosophy without discussing it first — revisit if the posterior
#      for b2 looks prior-dominated or implausibly wide.
#   k  (knot location): unchanged from the first draft — normal(24, 10),
#      centred near what earth found, wide enough for the data to override.
# ------------------------------------------------------------------------------
segmented_priors <- c(
  brms::prior(normal(0, 20), nlpar = "a"),
  brms::prior(normal(0, 20), nlpar = "b1"),
  brms::prior(normal(0, 20), nlpar = "b2"),
  brms::prior(normal(24, 10), nlpar = "k")
)

# ------------------------------------------------------------------------------
# SANITY CHECK — run this block FIRST. Does not fit anything, just generates
# and prints Stan code so you can visually confirm the formula compiles to
# something sensible before committing to a full sampling run.
# ------------------------------------------------------------------------------
stan_check <- brms::make_stancode(
  segmented_formula,
  data = all_data,
  prior = segmented_priors
)
cat(stan_check)
# If this errors out or produces something that looks obviously wrong,
# STOP HERE and we debug the formula together before fitting anything.

# ------------------------------------------------------------------------------
# SECOND SANITY CHECK — also run BEFORE the full fit. This formula previously
# failed post-fit (not at compile time) because fmin()/fmax() are valid Stan
# functions but don't exist in R, breaking loo()/fitted() which need to
# re-evaluate the nonlinear formula in R. step() is believed to be handled
# correctly by brms for this exact purpose, but that belief hasn't been
# directly verified — given two prior R/Stan mismatches on this formula,
# don't trust it a third time without checking. Confirm step() is
# R-evaluable in the way brms needs before running the full 6-chain fit:
#
#   brms:::step_predictor(c(-1, 0, 1))   # or similar — check ?brms:::step
#
# If unsure which internal helper applies, a simpler proxy check: fit a
# TINY, fast version of this same model (e.g. warmup=200, iterations=200,
# chains=1) first, then immediately try loo() or fitted() on it. If that
# small fit's post-processing works, the full run will too — this costs a
# couple of minutes instead of another ~8-minute-per-chain full run wasted
# on a repeat of the same failure mode.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Full fit (only run after the sanity check above looks right)
#
# Nonlinear/hinge models can mix more slowly than the simple models in
# script 01, especially if the knot posterior is multimodal or the sampler
# struggles near the hinge itself. Starting adapt_delta higher than the
# package default (0.95) is a reasonable precaution here specifically —
# 0.99 costs some speed but meaningfully reduces divergent-transition risk
# for exactly this kind of model.
#
# The first full run of this model (fmin/fmax version, same substantive
# formula) showed 4474 post-warmup transitions exceeding the default
# max_treedepth of 10. A second attempt (step() version, tiny 1-chain/200-
# draw proxy fit) showed the sampler landing on a nonsensical knot value
# (k = -3.39, outside VVIQ's 16-80 range) with catastrophic ESS — more
# consistent with a poor default initialization point than pure
# under-sampling, since brms's naive default init (near 0) is nowhere near
# a plausible knot location. Explicit inits (below) address that directly;
# max_treedepth raised to 15 as a further precaution in case the posterior
# genuinely needs more steps per iteration even from a good starting point.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Explicit initial values for the nonlinear parameters.
#
# Centred on sensible starting guesses (matching the priors' own centres,
# not fitted values), so the sampler starts in a plausible region instead
# of wherever brms's naive default init happens to land. Must be a
# FUNCTION returning a list (brms/rstan convention), applied per chain.
# ------------------------------------------------------------------------------
segmented_inits <- function() {
  list(
    b_a  = array(mean(all_data$tas)),  # intercept ~ near sample mean of TAS
    b_b1 = array(0),                   # slope ~ start neutral
    b_b2 = array(0),                   # slope-change ~ start neutral
    b_k  = array(24)                   # knot ~ prior centre, matches earth's finding
  )
}
# ------------------------------------------------------------------------------
segmented_estimated <-
  fit_brms_model(
    formula = segmented_formula,
    data = all_data,
    prior = segmented_priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.99,  # higher than default — see rationale above
    max_treedepth = 15,  # raised again — see rationale above
    init = segmented_inits,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "segmented_estimated_knot_tot.rds")
  )

# ------------------------------------------------------------------------------
# Diagnostics — especially important here given the nonlinear parameterisation
# ------------------------------------------------------------------------------
cat("\n--- Rhat / ESS check: segmented_estimated ---\n")
print(brms::rhat(segmented_estimated))
cat(sprintf("Min bulk ESS ~ %.0f\n",
            min(brms::neff_ratio(segmented_estimated) * brms::ndraws(segmented_estimated), na.rm = TRUE)))

# cat("\n--- Number of divergent transitions ---\n")
# print(brms::nuts_params(segmented_estimated, pars = "divergent__") |> sum())

# ------------------------------------------------------------------------------
# The headline result: posterior distribution of the knot location itself.
# This is the "classy, methodologically relevant alternative to Kvamme's
# arbitrary fixed threshold" Maël anticipated — report as a point estimate
# (posterior mean/median) with a 95% credible interval.
# ------------------------------------------------------------------------------
knot_posterior <- brms::as_draws_df(segmented_estimated, variable = "b_k_Intercept")
cat("\n--- Posterior summary: estimated knot location ---\n")
print(quantile(knot_posterior$b_k_Intercept, probs = c(0.025, 0.5, 0.975)))

# ------------------------------------------------------------------------------
# Segment slopes for reporting. Under this step()-based parameterisation,
# b1 IS the below-knot slope directly, but the above-knot slope is NOT b2 on
# its own — it's the derived quantity (b1 + b2). Compute both explicitly
# from posterior draws so the reported credible intervals correctly
# propagate uncertainty from both parameters, rather than naively adding
# point estimates.
# ------------------------------------------------------------------------------
slope_draws <- brms::as_draws_df(
  segmented_estimated,
  variable = c("b_b1_Intercept", "b_b2_Intercept")
)
slope_draws$below_knot_slope <- slope_draws$b_b1_Intercept
slope_draws$above_knot_slope <- slope_draws$b_b1_Intercept + slope_draws$b_b2_Intercept

cat("\n--- Below-knot slope (b1) ---\n")
print(quantile(slope_draws$below_knot_slope, probs = c(0.025, 0.5, 0.975)))
cat("\n--- Above-knot slope (b1 + b2) ---\n")
print(quantile(slope_draws$above_knot_slope, probs = c(0.025, 0.5, 0.975)))

# ------------------------------------------------------------------------------
# Add this model into the comparison table from script 01 (source that
# script's models first, or reload their saved .rds files, before running
# this comparison — kept as a separate step rather than duplicating the
# fits here).
# ------------------------------------------------------------------------------
lm_linear      <- readRDS(paste0(COMPARISON_MODEL_DIR, "lm_linear_tot.rds"))
lm_categorical <- readRDS(paste0(COMPARISON_MODEL_DIR, "lm_categorical_tot.rds"))
gam_tot        <- readRDS(paste0(COMPARISON_MODEL_DIR, "gam_tot.rds"))
segmented_fixed <- readRDS(paste0(COMPARISON_MODEL_DIR, "segmented_fixed_knot24_tot.rds"))

comparison_full <- compare_models_loo(
  list(
    linear               = lm_linear,
    categorical          = lm_categorical,
    gam                  = gam_tot,
    segmented_fixed      = segmented_fixed,
    segmented_estimated  = segmented_estimated
  )
)
print(comparison_full)