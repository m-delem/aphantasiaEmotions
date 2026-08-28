# ---------------------------------------------------------------------------- #
# PROBE — test modelbased/marginaleffects compatibility before building the
# full pd/ROPE script
# ---------------------------------------------------------------------------- #
#
# Purpose: floor_group_additive is a standard linear brms model (categorical
# x continuous structure, no nl=TRUE) — modelbased/marginaleffects should
# handle it exactly as they handle the existing categorical/GAM models, no
# special risk expected there.
#
# segmented_estimated IS an nl=TRUE nonlinear brms model. There is a
# documented brms GitHub issue (paul-buerkner/brms#925) showing
# conditional_effects() — which several modelbased functions wrap internally
# — having problems with nl=TRUE models in some formula structures. This
# probe tests our SPECIFIC model directly rather than assuming either way.
#
# Run this BEFORE the full pd/ROPE script. Report back what happens (works
# cleanly / errors / produces something that looks wrong) for each block.

library(brms)
library(bayestestR)
library(modelbased)

floor_group_additive <- readRDS("inst/models/floor_group_additive_multilevel_tot.rds")
segmented_estimated  <- readRDS("inst/models/segmented_estimated_knot_tot.rds")

# ---------------------------------------------------------------------------- #
# Test 1: floor-group model ----
# TEST 1: floor_group_additive — expected to work fine (standard linear
# model), included mainly as a baseline/control to confirm nothing broke.
# ---------------------------------------------------------------------------- #
cat("=== TEST 1: floor_group_additive, pd/ROPE on complete_aphant coefficient ===\n")
test1 <- tryCatch({
  bayestestR::p_direction(floor_group_additive, parameters = "complete_aphant")
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test1)

# ---------------------------------------------------------------------------- #
# Test 2: segmented, native parameters ----
# TEST 2: segmented_estimated — direct pd on native model parameters
# (b_k_Intercept, b_b1_Intercept, b_b2_Intercept). This does NOT go through
# conditional_effects()/modelbased — bayestestR::p_direction() on a brmsfit
# object typically works directly from posterior draws regardless of
# nl=TRUE, so this specific call is expected to be safe. Testing to confirm.
# ---------------------------------------------------------------------------- #
cat("\n=== TEST 2: segmented_estimated, pd on native nonlinear parameters ===\n")
test2 <- tryCatch({
  bayestestR::p_direction(
    segmented_estimated,
    parameters = c("b_k_Intercept", "b_b1_Intercept", "b_b2_Intercept")
  )
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test2)

# ---------------------------------------------------------------------------- #
# Test 3: segmented via estimate_slopes() ----
# TEST 3: segmented_estimated via modelbased::estimate_slopes() — THIS is
# the risky one, closest to the documented brms issue (conditional_effects-
# style functions on nl=TRUE models). May fail or misbehave.
# ---------------------------------------------------------------------------- #
cat("\n=== TEST 3: segmented_estimated, modelbased::estimate_slopes() ===\n")
test3 <- tryCatch({
  modelbased::estimate_slopes(segmented_estimated, trend = "vviq", by = "vviq", length = 10)
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test3)

# ---------------------------------------------------------------------------- #
# Test 4: derived above-knot slope ----
# TEST 4: pd/ROPE on the DERIVED above-knot slope (b1 + b2), computed
# manually from posterior draws — this sidesteps modelbased entirely and
# should be robust regardless of what tests 2-3 show, since it only uses
# raw posterior draws and base bayestestR functions on a plain numeric
# vector.
# ---------------------------------------------------------------------------- #
cat("\n=== TEST 4: derived above-knot slope (b1+b2), manual posterior draws ===\n")
test4 <- tryCatch({
  draws <- brms::as_draws_df(segmented_estimated, variable = c("b_b1_Intercept", "b_b2_Intercept"))
  above_knot_slope <- draws$b_b1_Intercept + draws$b_b2_Intercept
  list(
    pd = bayestestR::p_direction(above_knot_slope),
    rope = bayestestR::rope(above_knot_slope, range = c(-1, 1))  # placeholder ROPE range
  )
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test4)

cat("\n=== Probe complete (original 4 tests). Follow-up tests below. ===\n")

# ---------------------------------------------------------------------------- #
# Follow-up 1b: print method ----
# FOLLOW-UP TEST 1b: Test 1 showed only pd, no estimate/CI — check if this is
# a print-method default (verbose/full argument) rather than missing data.
# ---------------------------------------------------------------------------- #
cat("\n=== TEST 1b: floor_group_additive, describe_posterior (fuller output) ===\n")
test1b <- tryCatch({
  bayestestR::describe_posterior(
    floor_group_additive,
    parameters = "complete_aphant",
    rope_range = bayestestR::rope_range(floor_group_additive)
  )
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test1b)

# ---------------------------------------------------------------------------- #
# Follow-up 2b: parameter matching ----
# FOLLOW-UP TEST 2b: Test 2 returned only ONE parameter (labelled "Intercept")
# despite requesting three — check if `parameters` was treated as regex
# (matching only the first hit) rather than an exact vector. Try exact
# per-parameter calls instead of a combined vector.
# ---------------------------------------------------------------------------- #
cat("\n=== TEST 2b: segmented_estimated, one parameter at a time ===\n")
for (p in c("b_k_Intercept", "b_b1_Intercept", "b_b2_Intercept")) {
  cat(sprintf("\n--- %s ---\n", p))
  result <- tryCatch({
    bayestestR::p_direction(segmented_estimated, parameters = p)
  }, error = function(e) {
    cat("ERRORED:", conditionMessage(e), "\n")
    NULL
  })
  print(result)
}

# Also try describe_posterior on all three at once, which may handle the
# `parameters` argument differently than p_direction does
cat("\n--- describe_posterior, all three at once ---\n")
test2c <- tryCatch({
  bayestestR::describe_posterior(
    segmented_estimated,
    parameters = c("b_k_Intercept", "b_b1_Intercept", "b_b2_Intercept"),
    rope_range = bayestestR::rope_range(segmented_estimated)
  )
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
print(test2c)

cat("\n=== Follow-up probe complete. ===\n")
