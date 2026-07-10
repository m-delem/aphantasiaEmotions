# ==============================================================================
# Multilevel floor-group model — TAS-20 subscales
# ==============================================================================
#
# Extends the validated floor_group_additive_multilevel model (script 06,
# canonical formula: tas ~ vviq + complete_aphant + (vviq | study)) from
# total TAS-20 to its three subscales: Difficulty Identifying Feelings
# (tas_identify), Difficulty Describing Feelings (tas_describe), and
# Externally-Oriented Thinking (tas_external).
#
# Purpose: the total-TAS floor effect is this project's central finding.
# Whether that effect holds uniformly across all three subscales, or is
# concentrated in specific facets of alexithymia, in an open
# question worth answering for the main analyses.
#
# Same iteration/chain/adapt_delta/max_treedepth settings as the validated
# total-TAS multilevel model (script 06), since these three models share
# the same formula structure and are expected to need similar sampler
# accommodation.

source("inst/scripts/00_model_comparison_setup.R")

model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")
)

# ------------------------------------------------------------------------------
# Fit all three subscale models
# ------------------------------------------------------------------------------
floor_group_additive_multilevel_dif <-
  fit_brms_model(
    formula = tas_identify ~ vviq + complete_aphant + (vviq | study),
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.999,
    max_treedepth = 12,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_multilevel_dif.rds")
  )

floor_group_additive_multilevel_ddf <-
  fit_brms_model(
    formula = tas_describe ~ vviq + complete_aphant + (vviq | study),
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.999,
    max_treedepth = 12,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_multilevel_ddf.rds")
  )

floor_group_additive_multilevel_eot <-
  fit_brms_model(
    formula = tas_external ~ vviq + complete_aphant + (vviq | study),
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.999,
    max_treedepth = 12,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_multilevel_eot.rds")
  )

subscale_models <- list(
  DIF = floor_group_additive_multilevel_dif,
  DDF = floor_group_additive_multilevel_ddf,
  EOT = floor_group_additive_multilevel_eot
)

# ------------------------------------------------------------------------------
# Diagnostics — same pattern as script 06. Check BEFORE trusting these
# models; multilevel models can show poor Rhat/ESS on group-level SD/
# correlation terms even when fixed effects look fine.
# ------------------------------------------------------------------------------
cat("=== Subscale models: Rhat / ESS ===\n\n")
for (nm in names(subscale_models)) {
  m <- subscale_models[[nm]]
  rhats <- brms::rhat(m)
  ess_bulk <- brms::neff_ratio(m) * brms::ndraws(m)
  cat(sprintf(
    "%-6s max Rhat = %.4f | min bulk ESS ~ %.0f\n",
    nm, max(rhats, na.rm = TRUE), min(ess_bulk, na.rm = TRUE)
  ))
}

# ------------------------------------------------------------------------------
# Consolidated results table — floor-group coefficient (complete_aphantfloor)
# and above-floor vviq slope for each subscale, using describe_posterior()
# to match parameter_evidence.R's established approach. ROPE ranges use the
# SAME two-convention split as the total-TAS model (see
# parameter_evidence.R / implementation-notes.Rmd for the full rationale):
# rope_range() default for the floor-group CONTRAST, a Cohen-rescaled range
# for the SLOPE. Each subscale has its OWN SD, so both ROPE ranges are
# recomputed per subscale rather than reused from the total-TAS model.
# ------------------------------------------------------------------------------
subscale_outcomes <- list(
  DIF = "tas_identify",
  DDF = "tas_describe",
  EOT = "tas_external"
)

subscale_results <- do.call(rbind, lapply(names(subscale_models), function(nm) {
  m <- subscale_models[[nm]]
  outcome_var <- subscale_outcomes[[nm]]

  sd_outcome <- stats::sd(model_data[[outcome_var]])
  sd_vviq    <- stats::sd(model_data$vviq)
  rope_range_contrast <- bayestestR::rope_range(m)
  rope_range_slope     <- 0.2 * (sd_outcome / sd_vviq)

  floor_effect <- bayestestR::describe_posterior(
    m, parameters = "complete_aphant", rope_range = rope_range_contrast
  )
  vviq_slope <- bayestestR::describe_posterior(
    m, parameters = "vviq", rope_range = c(-rope_range_slope, rope_range_slope)
  )

  data.frame(
    subscale = nm,
    parameter = c("floor_effect", "vviq_slope"),
    median = c(floor_effect$Median, vviq_slope$Median),
    ci_low = c(floor_effect$CI_low, vviq_slope$CI_low),
    ci_high = c(floor_effect$CI_high, vviq_slope$CI_high),
    pd = c(floor_effect$pd, vviq_slope$pd),
    rope_low = c(rope_range_contrast[1], -rope_range_slope),
    rope_high = c(rope_range_contrast[2], rope_range_slope),
    pct_in_rope = c(floor_effect$ROPE_Percentage, vviq_slope$ROPE_Percentage)
  )
}))

cat("\n=== Consolidated subscale results ===\n\n")
print(subscale_results)

saveRDS(subscale_results, "inst/results/floor_group_subscale_results.rds")

cat("\nCheck specifically: does the floor effect (complete_aphant) stay\n")
cat("clearly non-zero across all three subscales, or is it concentrated in\n")
cat("specific facets of alexithymia? This is the open question this script\n")
cat("was written to answer.\n")
