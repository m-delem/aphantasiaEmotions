# ==============================================================================
# Model diagnostics and comparison — single consolidated pass
# ==============================================================================
#
# Loads every model fit by scripts 01-04 and runs, in one place:
#   1. Convergence diagnostics (Rhat, ESS) for every model
#   2. Posterior predictive checks for every model
#   3. Family-choice diagnostics (justifying gaussian() — supersedes the
#      earlier standalone 03_family_choice_diagnostics.R, which should be
#      considered retired once this script is in use)
#   4. The full LOO/WAIC comparison table across all fitted models
#
# Run this AFTER scripts 01-04 (or whichever subset of them you've fit).
# Models not yet fit are skipped gracefully with a message, so this script
# can be run incrementally as new models are added.

source("inst/analysis/00_model_comparison_setup.R")

# ------------------------------------------------------------------------------
# Load all models that exist on disk. Silently skips any not yet fit, with
# a message, rather than erroring — lets you run this after script 01 alone,
# or after all of 01-04, without editing this script each time.
# ------------------------------------------------------------------------------
model_files <- c(
  linear                  = "lm_linear_tot.rds",
  categorical             = "lm_categorical_tot.rds",
  cat_2_groups             = "lm_categorical_2_group_tot.rds",
  gam                     = "gam_tot.rds",
  segmented_fixed         = "segmented_fixed_knot_tot.rds",
  segmented_estimated     = "segmented_estimated_knot_tot.rds",
  floor_group_additive    = "floor_group_additive_tot.rds"
)

models <- list()
for (nm in names(model_files)) {
  path <- paste0(COMPARISON_MODEL_DIR, model_files[nm])
  if (file.exists(path)) {
    models[[nm]] <- readRDS(path)
  } else {
    message(sprintf("Skipping '%s': %s not found yet.", nm, path))
  }
}

cat(sprintf("\nLoaded %d of %d possible models: %s\n\n",
            length(models), length(model_files), paste(names(models), collapse = ", ")))

if (length(models) == 0) stop("No fitted models found in ", COMPARISON_MODEL_DIR)

# ==============================================================================
# 1. Convergence diagnostics — Rhat and ESS for every loaded model
# ==============================================================================
cat("=== Convergence diagnostics (Rhat / ESS) ===\n\n")
for (nm in names(models)) {
  m <- models[[nm]]
  rhats <- brms::rhat(m)
  ess_bulk <- brms::neff_ratio(m) * brms::ndraws(m)
  cat(sprintf(
    "%-24s max Rhat = %.4f | min bulk ESS ~ %.0f\n",
    nm, max(rhats, na.rm = TRUE), min(ess_bulk, na.rm = TRUE)
  ))
}
cat("\nIf any max Rhat > 1.01 or min ESS < 400: that model's fit should be\n")
cat("re-run with more iterations (or investigated further) before trusting\n")
cat("its results or its place in the comparison table below.\n\n")

# ==============================================================================
# 2. Posterior predictive checks — saved as objects for reuse in the EOR
#    later, in addition to being available for interactive viewing now.
# ==============================================================================
cat("=== Posterior predictive checks ===\n")
cat("(Objects saved to disk; view interactively with plot(pp_checks$<name>))\n\n")

pp_checks <- lapply(models, function(m) {
  tryCatch(
    performance::check_predictions(m, draw_ids = 1:12),
    error = function(e) {
      message("check_predictions() failed for a model: ", conditionMessage(e))
      NULL
    }
  )
})
saveRDS(pp_checks, paste0(COMPARISON_MODEL_DIR, "pp_checks_all_models.rds"))

# ==============================================================================
# 3. Family-choice diagnostics (justifying gaussian() for TAS total)
#
# Supersedes the earlier standalone 03_family_choice_diagnostics.R.
# Rationale: TAS-20 total is a sum of 20 five-point items (range 20-100 in
# principle), bounded and technically discrete, but with enough range for a
# continuous approximation to be standard in this literature. These checks
# don't "prove" gaussian() is correct — they surface any obvious problem
# (skew, heteroscedasticity, boundary pile-up) that would call for a
# bounded/skewed alternative instead.
# ==============================================================================
cat("\n=== Family choice diagnostics (gaussian() for TAS total) ===\n\n")

tas_vals <- models[[1]]$data$tas  # same outcome vector across all models
cat(sprintf("Raw TAS range: [%.1f, %.1f]\n", min(tas_vals), max(tas_vals)))
cat(sprintf("Raw TAS skewness: %.3f (near 0 = symmetric)\n",
            mean((tas_vals - mean(tas_vals))^3) / sd(tas_vals)^3))
cat(sprintf("Any values at theoretical boundaries (20 or 100)? %s\n\n",
            any(tas_vals <= 20 | tas_vals >= 100)))

cat("Residual skewness per model (near 0 = symmetric):\n")
for (nm in names(models)) {
  resid_vals <- residuals(models[[nm]])[, "Estimate"]
  skew <- mean((resid_vals - mean(resid_vals))^3) / sd(resid_vals)^3
  cat(sprintf("%-24s skewness = %.3f\n", nm, skew))
}

cat("\n|Residual| vs fitted value correlation (near 0 = homoscedastic):\n")
for (nm in names(models)) {
  fitted_vals <- fitted(models[[nm]])[, "Estimate"]
  resid_vals  <- residuals(models[[nm]])[, "Estimate"]
  cat(sprintf("%-24s r = %.3f\n", nm, cor(abs(resid_vals), fitted_vals)))
}

# ==============================================================================
# 4. WAIC/LOO comparison across all loaded models
# ==============================================================================
cat("\n=== Model comparison (LOO, WAIC fallback) ===\n\n")

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
  
  pareto_flags <- vapply(loo_list, function(l) {
    if (!is.null(l$diagnostics$pareto_k)) sum(l$diagnostics$pareto_k > 0.7) else NA_integer_
  }, numeric(1))
  
  comp <- brms::loo_compare(loo_list)
  comp_df <- as.data.frame(comp)
  comp_df$model <- rownames(comp_df)
  comp_df$n_high_pareto_k <- pareto_flags[comp_df$model]
  rownames(comp_df) <- NULL
  
  comp_df[, c("model", "elpd_diff", "se_diff", "n_high_pareto_k")]
}

comparison_all <- compare_models_loo(models)
print(comparison_all)

saveRDS(comparison_all, paste0(COMPARISON_MODEL_DIR, "comparison_all_models_tot.rds"))
write.csv(
  comparison_all,
  paste0(COMPARISON_MODEL_DIR, "comparison_all_models_tot.csv"),
  row.names = FALSE
)

cat("\nDone. Comparison table saved to", COMPARISON_MODEL_DIR, "\n")