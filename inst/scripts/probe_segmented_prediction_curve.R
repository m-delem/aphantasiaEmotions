# ==============================================================================
# PROBE — segmented_estimated prediction-curve compatibility
# ==============================================================================
#
# Purpose: the linear/GAM/segmented overlay figure needs a full prediction
# CURVE (across a range of vviq values) from all three models, not just a
# single point or a slope at specific values. estimate_slopes() was already
# confirmed safe on segmented_estimated (nl=TRUE) in
# probe_modelbased_compatibility.R, but that's a DIFFERENT operation from
# generating a smooth prediction curve across a full range — this has not
# been tested. Given the documented brms GitHub issue about
# conditional_effects()-style functions and nl=TRUE models, this is worth
# checking before building the overlay figure around an assumption.
#
# This mirrors plot_floor_group()'s already-working approach
# (marginaleffects::predictions() on a data.frame grid) applied to
# segmented_estimated specifically, since that's the exact mechanism the
# overlay figure would need to reuse.

library(brms)
library(marginaleffects)
library(modelbased)

segmented_estimated <- readRDS("inst/models/segmented_estimated_knot_tot.rds")

# ------------------------------------------------------------------------------
# TEST 1: marginaleffects::predictions() across a full vviq range —
# the same mechanism plot_floor_group() already uses successfully on a
# LINEAR model. Testing here on the nl=TRUE segmented model specifically.
# ------------------------------------------------------------------------------
cat("=== TEST 1: marginaleffects::predictions(), full range ===\n")
pred_grid <- data.frame(vviq = seq(16, 80, length.out = 50))

test1 <- tryCatch({
  marginaleffects::predictions(segmented_estimated, newdata = pred_grid)
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(test1)) {
  test1_df <- as.data.frame(test1)
  cat("Succeeded. First few rows:\n")
  print(head(test1_df[, c("vviq", "estimate", "conf.low", "conf.high")]))
  cat("\nSpot-check: does the curve actually bend near the known knot (~19.5)?\n")
  print(test1_df[test1_df$vviq > 15 & test1_df$vviq < 30, c("vviq", "estimate")])
}

# ------------------------------------------------------------------------------
# TEST 2: modelbased::estimate_means() — the alternative mechanism used by
# plot_gam_means() for the previous manuscript's GAM figure. Testing whether 
# this ALSO works on the nl=TRUE model, as a second option in case Test 1 has 
# issues.
# ------------------------------------------------------------------------------
cat("\n=== TEST 2: modelbased::estimate_means(), full range ===\n")
test2 <- tryCatch({
  modelbased::estimate_means(segmented_estimated, by = "vviq", length = 50)
}, error = function(e) {
  cat("ERRORED:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(test2)) {
  cat("Succeeded. First few rows:\n")
  print(head(as.data.frame(test2)))
}

# ------------------------------------------------------------------------------
# TEST 3: sanity cross-check — does Test 1's curve, if it worked, actually
# match the known coefficients (b1=2.46 below knot, b1+b2=-0.28 above,
# knot~19.5)? A working function call that produces WRONG numbers would be
# worse than an honest error — checking the shape, not just "did it run".
# ------------------------------------------------------------------------------
if (!is.null(test1)) {
  cat("\n=== TEST 3: sanity-check curve shape against known coefficients ===\n")
  test1_df <- as.data.frame(test1)
  # Rough slope check: compare predicted values just below and just above
  # the known knot (~19.5) to see if the direction/magnitude looks right
  below <- test1_df[which.min(abs(test1_df$vviq - 17)), ]
  above <- test1_df[which.min(abs(test1_df$vviq - 60)), ]
  cat(sprintf("Prediction at vviq~17: %.2f\n", below$estimate))
  cat(sprintf("Prediction at vviq~60: %.2f\n", above$estimate))
  cat("Expected: a peak/high point near the knot (~19.5), declining toward\n")
  cat("higher vviq values (consistent with below-knot positive slope, \n")
  cat("above-knot negative slope ~-0.28) — visually inspect the full printed\n")
  cat("curve above to confirm this shape, not just these two points.\n")
}

cat("\n=== Probe complete. Report back which tests worked and whether the\n")
cat("curve shape looks right (bends near ~19.5, matches known slopes). ===\n")
