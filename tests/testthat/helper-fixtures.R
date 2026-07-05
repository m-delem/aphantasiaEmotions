# ==============================================================================
# Shared test fixture — one tiny brms model, reused across test files
# ==============================================================================
#
# Fitting a real brms model is expensive (Stan compilation + sampling).
# Fitting one small, fast model here and reusing it across test files that
# need a real fitted brms object keeps CI cost to one compile, not several.
# Iteration counts are deliberately minimal (just enough to produce a
# valid fitted object with sane structure) — NOT meant to represent a real
# analysis, only to exercise the code paths that consume a fitted brms
# model. This intentionally under-powered setup reliably triggers rstan's
# post-hoc ESS/Rhat warnings (accurate, expected, not indicative of a real
# problem) — suppressWarnings() here targets exactly those, without
# risking silencing an unrelated, genuinely informative warning elsewhere
# in the test suite (which a blanket options(warn = -1) would risk doing).

.make_test_floor_model <- function() {
  model_data <- all_data
  model_data$complete_aphant <- factor(
    ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
    levels = c("above_floor", "floor")
  )
  suppressWarnings(
    fit_brms_model(
      formula = tas ~ vviq + complete_aphant,
      data = model_data,
      prior = brms::prior(normal(0, 20), class = "b"),
      chains = 1,
      iterations = 100,
      warmup = 100,
      refresh = 0,
      file = NULL
    )
  )
}