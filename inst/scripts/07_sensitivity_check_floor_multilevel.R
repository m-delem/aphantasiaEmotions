# ---------------------------------------------------------------------------- #
# Prior sensitivity check — group-level slope SD, floor_group_additive_multilevel
# ---------------------------------------------------------------------------- #
#
# Purpose: floor_group_additive_multilevel's (vviq | study) random slope
# relies on brms's default weakly-informative prior for the group-level SD
# term (sd(vviq) by study), chosen deliberately over a hand-picked prior. This 
# script checks whether the model's substantive conclusions are robust to that 
# choice, by refitting with a deliberately WIDER prior and comparing.
#
# Deliberately EOR-only — one manuscript sentence ("results were robust to
# alternative prior specifications, see EOR") is the intended footprint,
# not a manuscript table/figure.

source("inst/scripts/00_model_comparison_setup.R")

floor_group_additive_multilevel <-
  readRDS("inst/models/floor_group_additive_multilevel_tot.rds")

# ---------------------------------------------------------------------------- #
# Step 1: the default prior ----
# Step 1: confirm the ACTUAL default prior used, rather than assume its
# exact form from memory. brms::prior_summary() reads directly off the
# already-fitted model object.
# ---------------------------------------------------------------------------- #
cat("=== Default prior actually used (from the fitted model) ===\n")
print(brms::prior_summary(floor_group_additive_multilevel))

# ---------------------------------------------------------------------------- #
# Step 2: refit with a wider prior ----
# Step 2: refit with an explicit, deliberately WIDER prior on the
# group-level slope SD specifically — class = "sd", group = "study",
# coef = "vviq" targets exactly the (vviq | study) term, confirmed via
# brms documentation (set_prior() reference) rather than guessed.
#
# All other priors (fixed effects, intercept SD, correlation) are left
# at whatever this model's existing `priors` object + brms defaults
# already specify — only the ONE term under test changes.
# ---------------------------------------------------------------------------- #
sensitivity_priors <- c(
  brms::prior(
    normal(0, 40), class = "b"), # twice as wide as our normal(0,20) default
  brms::prior(
    student_t(3, 0, 26.6), # twice as wide as brms' default (13.3)
    class = "sd", group = "study", coef = "vviq")
)

model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")
)

floor_group_additive_multilevel_wide_prior <-
  fit_brms_model(
    formula = tas ~ vviq + complete_aphant + (vviq | study),
    data = model_data,
    prior = sensitivity_priors,
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.999,  # matching what 05/06 needed for clean convergence
    max_treedepth = 12,
    file_refit = "on_change",
    file = paste0(COMPARISON_MODEL_DIR, "floor_group_additive_multilevel_wide_prior_tot.rds")
  )

# ---------------------------------------------------------------------------- #
# Step 3: diagnostics ----
# Step 3: diagnostics — confirm the wider-prior version also converged
# cleanly before trusting any comparison.
# ---------------------------------------------------------------------------- #
cat("\n=== Wide-prior model: Rhat / ESS ===\n")
print(brms::rhat(floor_group_additive_multilevel_wide_prior))
cat(sprintf("\nMin bulk ESS ~ %.0f\n",
    min(brms::neff_ratio(floor_group_additive_multilevel_wide_prior) *
        brms::ndraws(floor_group_additive_multilevel_wide_prior), na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
# Step 4: default vs wide prior ----
# Step 4: the actual comparison — default vs. wide prior, side by side.
# Focus on: (a) the group-level slope SD itself (directly affected by the
# prior change), (b) the fixed-effect vviq slope, (c) the floor-group
# coefficient (the paper's headline effect) — (b) and (c) are what
# actually matters for "is our conclusion prior-robust."
# ---------------------------------------------------------------------------- #
default_fixef <- brms::fixef(floor_group_additive_multilevel)
wide_fixef    <- brms::fixef(floor_group_additive_multilevel_wide_prior)

default_sd_vviq <- brms::VarCorr(floor_group_additive_multilevel)$study$sd["vviq", "Estimate"]
wide_sd_vviq     <- brms::VarCorr(floor_group_additive_multilevel_wide_prior)$study$sd["vviq", "Estimate"]

cat("\n=== Sensitivity comparison: default vs. wide prior on sd(vviq | study) ===\n\n")
cat(sprintf("%-25s %-10s %-10s\n", "Parameter", "Default", "Wide prior"))
cat(sprintf("%-25s %-10.3f %-10.3f\n", "sd(vviq | study)", default_sd_vviq, wide_sd_vviq))
cat(sprintf("%-25s %-10.3f %-10.3f\n", "vviq (fixed slope)",
    default_fixef["vviq", "Estimate"], wide_fixef["vviq", "Estimate"]))
cat(sprintf("%-25s %-10.3f %-10.3f\n", "complete_aphantfloor",
    default_fixef["complete_aphantfloor", "Estimate"], wide_fixef["complete_aphantfloor", "Estimate"]))

cat("\nIf 'vviq' and 'complete_aphantfloor' rows are close between the two\n")
cat("columns (relative to their own CIs), the headline floor effect is\n")
cat("robust to this prior choice — the substantive conclusion doesn't\n")
cat("depend on which weakly-informative prior was used for the group-level\n")
cat("slope SD. A meaningful shift would be worth understanding before\n")
cat("claiming robustness in the manuscript sentence.\n")
