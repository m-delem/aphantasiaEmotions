# ==============================================================================
# Shared test fixture — one tiny brms model, reused across test files
# ==============================================================================
#
# This model is NOT fit at test time. Compiling a Stan model requires a real,
# working Boost/g++ toolchain, which is a recurring source of CI breakage
# (rstan/StanHeaders/BH version drift — see implementation-notes.Rmd for the
# same class of problem in the main models) — entirely avoidable here, since
# tests only need *a* valid fitted brms object to exercise plotting code
# paths, not a freshly-compiled one. Instead, the fitted object is
# pre-computed once, locally, and checked into
# tests/testthat/fixtures/test_floor_model.rds — testthat's own convention
# for this exact situation (test data/objects that ship with the repo rather
# than being generated during the test run).
#
# Iteration counts used to produce the fixture were deliberately minimal
# (just enough for a valid fitted object with sane structure) — NOT meant to
# represent a real analysis, only to exercise the code paths that consume a
# fitted brms model.
#
# To regenerate the fixture after a formula/prior/data change, run once
# locally (NOT in CI):
#
# model_data <- all_data
# model_data$complete_aphant <- factor(
#   ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
#   levels = c("above_floor", "floor")
# )
# m <- fit_brms_model(
#   formula = tas ~ vviq + complete_aphant,
#   data = model_data,
#   prior = brms::prior(normal(0, 20), class = "b"),
#   chains = 1,
#   iterations = 100,
#   warmup = 100,
#   refresh = 0,
#   file = testthat::test_path("fixtures", "test_floor_model.rds")
# )

.make_test_floor_model <- function() {
  readRDS(testthat::test_path("fixtures", "test_floor_model.rds"))
}