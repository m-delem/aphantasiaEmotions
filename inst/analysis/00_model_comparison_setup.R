# ==============================================================================
# Model comparison setup — shared across all model-fitting scripts
# ==============================================================================
#
# This script defines the shared priors, iteration/warmup/chain constants used
# by scripts 01-04. Comparison and diagnostics logic itself lives in
# model_diagnostics_and_comparison.R, not here or in the individual fitting
# scripts — this script only prepares what's needed to FIT models
# consistently.
#
# Source this script at the top of each fitting script (01-04) before fitting.

library(aphantasiaEmotions)  # for fit_brms_model(), all_data
library(brms)

# ------------------------------------------------------------------------------
# Priors — matching the existing vignette convention (normal(0, 20) on all
# fixed effects, brms defaults elsewhere). No new prior philosophy introduced
# here; keeps this comparison cycle consistent with what's already
# prior-predictive-checked in the main vignette.
# ------------------------------------------------------------------------------
priors <- c(brms::prior(normal(0, 20), class = "b"))

# ------------------------------------------------------------------------------
# Iteration / warmup / chain budget for this comparison cycle.
#
# fit_brms_model() takes `iterations` as POST-WARMUP DRAWS PER CHAIN directly,
# with `chains` and `warmup` as separate, explicit arguments (see
# R/modelling_tools.R for the current definition). These constants make the
# comparison cycle's choices visible and easy to change in one place.
#
# CHAINS_COMPARISON = 6: a bit above the function's own default of 4, for
# extra convergence confidence on newer model forms (segmented, floor-group
# interaction) at low extra cost given a 24-core machine.
# ------------------------------------------------------------------------------
CHAINS_COMPARISON     <- 6
ITERATIONS_COMPARISON <- 2000 # post-warmup draws PER CHAIN
WARMUP_COMPARISON     <- 1000

# Adapt_delta stays at the package default (0.95) for the simpler models.
# Nonlinear/segmented models may need a higher value if divergences appear —
# see script 03 for the rationale behind raising it there specifically.
ADAPT_DELTA_DEFAULT <- 0.95

# Directory for saving fitted models from this comparison cycle. Kept
# SEPARATE from inst/models/ (the package's canonical, EOR/vignette-facing
# model store) since these are exploratory comparison fits, not yet the
# validated models that will be documented in the package. Promote the
# winning model(s) into inst/models/ once selection is done.
COMPARISON_MODEL_DIR <- "inst/analysis/models_comparison/"
if (!dir.exists(COMPARISON_MODEL_DIR)) {
  dir.create(COMPARISON_MODEL_DIR, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# FOLLOW-UP FLAGGED, NOT DONE HERE: the main package vignette's prose
# (aphantasiaEmotions.Rmd, "Bayesian setup" section) still describes the OLD
# fit_brms_model() behaviour ("24000 post-warmup iterations spread across all
# available CPU"). Now that `iterations` means post-warmup draws per chain
# and chains/cores are decoupled from detectCores(), that prose is stale.
# ------------------------------------------------------------------------------
