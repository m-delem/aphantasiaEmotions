# ==============================================================================
# Multilevel floor-group additive model — random intercept + slope by study
# Outcome: total TAS-20 score.
# ==============================================================================
#
# Formula: tas ~ vviq + complete_aphant + (vviq | study)
#
# Same random-effects structure and prior philosophy as
# 05_multilevel_linear.R (see that script for the full rationale on
# random-slope justification and default group-level priors) — this
# script wraps the floor_group_additive formula.
#
# NOTE: complete_aphant itself does NOT get a random slope by study here
# (i.e., no (complete_aphant | study) or interaction with it) — this
# mirrors the single-level model's own structure (complete_aphant as a
# fixed group-level shift) and avoids adding a second layer of untested
# complexity in the same step. If a study-varying floor effect is of
# separate interest later, treat that as its own explicit extension, not
# something folded in silently here.

source("inst/scripts/00_model_comparison_setup.R")

model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")
)

floor_group_additive_multilevel <-
  fit_brms_model(
    formula = tas ~ vviq + complete_aphant + (vviq | study),
    data = model_data,
    prior = priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.9999,
    max_treedepth = 12,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_multilevel_tot.rds")
  )

# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------
cat("=== floor_group_additive_multilevel: Rhat / ESS ===\n")
print(brms::rhat(floor_group_additive_multilevel))
cat(sprintf("\nMin bulk ESS ~ %.0f\n",
    min(brms::neff_ratio(floor_group_additive_multilevel) * brms::ndraws(floor_group_additive_multilevel), na.rm = TRUE)))

cat("\n=== Group-level (study) effects summary ===\n")
print(summary(floor_group_additive_multilevel)$random)

cat("\nCheck specifically: does the floor-group coefficient (complete_aphant)\n")
cat("stay close to the single-level estimate (-8.75 [-11.38, -6.11]) once\n")
cat("study heterogeneity is accounted for? A meaningful shift here would be\n")
cat("worth understanding before treating this as the headline manuscript\n")
cat("model.\n")

# ------------------------------------------------------------------------------
# NOTE: worth checking how floor-group participants are distributed across
# studies before over-interpreting this model's random slope estimates —
# the floor group being concentrated in only 3 studies (the aphantasia-focused
# datasets), the interaction between the (vviq | study) random effects and the 
# floor-group contrast could behave in ways worth a closer look, not assumed to 
# be clean.
# ------------------------------------------------------------------------------
cat("\n=== Floor-group participants per study (context for interpretation) ===\n")
print(table(model_data$study, model_data$complete_aphant))

cat("\n=== Sourcing the single-level floor model ================================\n")
source("inst/scripts/04_model_comparison_floor_group.R")

cat("\n=== Summary of the single-level floor model ===\n")
print(summary(floor_group_additive))

cat("\n=== Summary of the multilevel floor model ===\n")
print(summary(floor_group_additive_multilevel))

cat("---------------------------------------------------------------------------------\n")
cat("Script 06 done: floor_group_additive fit and saved to", COMPARISON_MODEL_DIR, "\n")
cat("then checked and compared with the results of the single-level model.\n")
cat("Run model_diagnostics_and_comparison.R for checks and comparison tables.\n")
cat("---------------------------------------------------------------------------------\n")