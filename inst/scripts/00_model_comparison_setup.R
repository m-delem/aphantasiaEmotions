# ---------------------------------------------------------------------------- #
# Model comparison setup — shared across all model-fitting scripts
# ---------------------------------------------------------------------------- #
#
# This script defines the shared priors, iteration/warmup/chain constants used
# by scripts 01-04. Comparison and diagnostics logic itself lives in
# model_diagnostics_and_comparison.R, not here or in the individual fitting
# scripts — this script only prepares what's needed to FIT models
# consistently.
#
# Source this script at the top of each fitting script (01-04) before fitting.

devtools::load_all()  # for fit_brms_model(), all_data
library(brms)

# ---------------------------------------------------------------------------- #
# Priors ----
# Priors — wide normal(0, 20) on all fixed effects, brms defaults elsewhere. 
# ---------------------------------------------------------------------------- #
priors <- c(brms::prior(normal(0, 20), class = "b"))

# ---------------------------------------------------------------------------- #
# Sampling budget ----
# Iteration / warmup / chain budget for this comparison cycle.
#
# fit_brms_model() takes `iterations` as POST-WARMUP DRAWS PER CHAIN directly,
# with `chains` and `warmup` as separate, explicit arguments (see
# R/modelling_tools.R for the current definition). These constants make the
# comparison cycle's choices visible and easy to change in one place.
#
# CHAINS_COMPARISON = 6: a bit above the function's own default of 4, for
# extra convergence confidence on newer model forms (segmented, floor-group
# interaction) at low extra cost given the 24-core machine (available to yours
# truly).
# ---------------------------------------------------------------------------- #
CHAINS_COMPARISON     <- 6
ITERATIONS_COMPARISON <- 2000 # post-warmup draws PER CHAIN
WARMUP_COMPARISON     <- 1000

# Adapt_delta stays at the package default (0.95) for the simpler models.
# Nonlinear/segmented models may need a higher value if divergences appear —
# see script 03 for the rationale behind raising it there specifically.
ADAPT_DELTA_DEFAULT <- 0.95

# Directory for saving fitted models.
COMPARISON_MODEL_DIR <- "inst/models/"
if (!dir.exists(COMPARISON_MODEL_DIR)) {
  dir.create(COMPARISON_MODEL_DIR, recursive = TRUE)
}