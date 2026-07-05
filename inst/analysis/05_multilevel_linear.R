# ==============================================================================
# Multilevel linear model — random intercept + slope by study
# Outcome: total TAS-20 score.
# ==============================================================================
#
# Formula: tas ~ vviq + (vviq | study)
#
# Random intercept AND slope, not intercept-only — per-study Ns support
# this (Burns 192, Monzel 105, Mas 123, Ruby 225, Kvamme 833 — all large
# enough to estimate each study's OWN slope well). The less-precisely-
# estimated quantity is the group-level SD of the slope across only 5
# studies (an acknowledged limitation), not each study's individual slope 
# estimate.
#
# Priors: brms DEFAULTS on group-level SD/correlation terms (deliberately
# not hand-tightened — with only 5 clusters, an informative-feeling prior
# on the group-level SD would do more inferential work than can be
# defended). Fixed-effect prior (vviq slope) matches the existing
# normal(0, 20) package convention. A sensitivity check (default vs. wider
# student_t(3,0,5) on the group-level slope SD) is planned as a separate,
# later, EOR-only step — not done in this script.

source("inst/analysis/00_model_comparison_setup.R")

lm_linear_multilevel <-
  fit_brms_model(
    formula = tas ~ vviq + (vviq | study),
    data = all_data,
    prior = priors,  # normal(0,20) on the fixed vviq slope; group-level
                     # terms use brms defaults
    iterations = ITERATIONS_COMPARISON,
    warmup = WARMUP_COMPARISON,
    chains = CHAINS_COMPARISON,
    adapt_delta = 0.999,
    max_treedepth = 12,
    file_refit = "always",
    file = paste0(COMPARISON_MODEL_DIR, "lm_linear_multilevel_tot.rds")
  )

# ------------------------------------------------------------------------------
# Diagnostics — check BEFORE trusting this model. Multilevel models can
# show poor Rhat/ESS specifically on the group-level SD/correlation terms
# even when fixed effects look fine — check those parameters explicitly,
# not just the overall max/min summary.
# ------------------------------------------------------------------------------
cat("=== lm_linear_multilevel: Rhat / ESS ===\n")
print(brms::rhat(lm_linear_multilevel))
cat(sprintf("\nMin bulk ESS ~ %.0f\n",
    min(brms::neff_ratio(lm_linear_multilevel) * brms::ndraws(lm_linear_multilevel), na.rm = TRUE)))

cat("\n=== Group-level (study) effects summary ===\n")
print(summary(lm_linear_multilevel)$random)

cat("\nCheck specifically: does the population-level (fixed) vviq slope\n")
cat("change meaningfully from the single-level lm_linear model once study\n")
cat("heterogeneity is accounted for? This is the 'with vs. without study'\n")
cat("question flagged early in this analysis cycle as needing your judgment.\n")
