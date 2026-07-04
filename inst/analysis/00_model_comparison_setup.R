# ==============================================================================
# Model comparison setup — shared across fixed-knot and estimated-knot scripts
# ==============================================================================
#
# This script defines the shared priors, iteration/warmup budget, and a
# WAIC/LOO comparison helper used by both:
#   - 01_model_comparison_fixed_knot.R
#   - 02_model_comparison_estimated_knot.R
#
# Source this script at the top of each of the above before fitting models.

library(aphantasiaEmotions)  # for fit_brms_model(), all_data
library(brms)

# ------------------------------------------------------------------------------
# Priors — matching the existing vignette convention (normal(0, 20) on all
# fixed effects, brms defaults elsewhere). No new prior philosophy introduced
# here; this keeps the model-comparison cycle consistent with what's already
# prior-predictive-checked in the main vignette.
# ------------------------------------------------------------------------------
priors <- c(brms::prior(normal(0, 20), class = "b"))

# ------------------------------------------------------------------------------
# Iteration / warmup / chain budget for THIS comparison cycle.
#
# fit_brms_model() now takes `iterations` as POST-WARMUP DRAWS PER CHAIN
# directly (no division by core count), with `chains` and `warmup` as
# separate, explicit arguments. These constants just make the comparison
# cycle's choices visible and easy to change in one place; they match the
# function's own defaults here; override per-model below only if diagnostics
# ask for it.
#
# CHAINS_COMPARISON = 6: a bit above the function's default of 4, since
# we're specifically interested in convergence stability across several new
# model forms (segmented, later multilevel) where extra chains give more
# confidence in Rhat/ESS at low extra cost given a 24-core machine.
# ------------------------------------------------------------------------------
CHAINS_COMPARISON     <- 6
ITERATIONS_COMPARISON <- 2000 # post-warmup draws PER CHAIN
WARMUP_COMPARISON     <- 1000

# Adapt_delta stays at the package default (0.95) for the simpler models.
# The segmented models (especially the estimated-knot version) may need a
# higher value if you see divergent transitions — try 0.99 for those
# specifically before touching anything else.
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
# WAIC/LOO comparison helper
# ------------------------------------------------------------------------------
#' Compare a named list of fitted brms models via LOO (with WAIC fallback)
#'
#' @param model_list A named list of fitted brms model objects, e.g.
#'   list(linear = lm_fit, categorical = cat_fit, gam = gam_fit, segmented = seg_fit)
#' @param moment_match Logical. If TRUE, uses brms::loo() with moment matching
#'   to reduce the risk of unreliable Pareto k diagnostics (slower, but more
#'   trustworthy — recommended once you have a short list of finalist models,
#'   not necessarily for a first broad pass).
#'
#' @returns A data frame summarising elpd_diff and se_diff across models,
#'   ordered from best to worst, plus a column flagging any high Pareto k
#'   warnings that suggest a LOO estimate may be unreliable for that model.
compare_models_loo <- function(model_list, moment_match = FALSE) {
  stopifnot(!is.null(names(model_list)), all(names(model_list) != ""))
  
  loo_list <- lapply(model_list, function(m) {
    tryCatch(
      brms::loo(m, moment_match = moment_match),
      error = function(e) {
        message("LOO failed for a model, falling back to WAIC: ", conditionMessage(e))
        brms::waic(m)
      }
    )
  })
  names(loo_list) <- names(model_list)
  
  # Flag any high Pareto k values (> 0.7) per model, if LOO (not WAIC) was used
  pareto_flags <- vapply(loo_list, function(l) {
    if (!is.null(l$diagnostics$pareto_k)) {
      sum(l$diagnostics$pareto_k > 0.7)
    } else {
      NA_integer_
    }
  }, numeric(1))
  
  comp <- brms::loo_compare(loo_list)
  comp_df <- as.data.frame(comp)
  comp_df$model <- rownames(comp_df)
  comp_df$n_high_pareto_k <- pareto_flags[comp_df$model]
  rownames(comp_df) <- NULL
  
  comp_df[, c("model", "elpd_diff", "se_diff", "n_high_pareto_k")]
}


# ------------------------------------------------------------------------------
# Sanity check: confirm `study` factor levels before any multilevel work later.
# Not used in the single-level comparison scripts, but worth eyeballing now.
# ------------------------------------------------------------------------------
# table(all_data$study)

# ------------------------------------------------------------------------------
# FOLLOW-UP FLAGGED, NOT DONE HERE: the main package vignette's prose
# (aphantasiaEmotions.Rmd, "Bayesian setup" section) still describes the OLD
# fit_brms_model() behaviour ("24000 post-warmup iterations spread across all
# available CPU"). Now that `iterations` means post-warmup draws per chain
# and chains/cores are decoupled from detectCores(), that prose is stale.
# ------------------------------------------------------------------------------
