# ---------------------------------------------------------------------------- #
# Kvamme et al. replication — split-sample regressions on THEIR actual data
# ---------------------------------------------------------------------------- #
#
# Purpose: Kvamme et al. (2026, Neuropsychologia) report Pearson correlations
# between VVIQ and TAS separately for their aphantasia group (VVIQ 16-32,
# n=153) and non-aphantasia group (VVIQ 33-80, n=680) — see their Table 1
# and Fig. 2. Since their raw data is included in all_data (study ==
# "kvamme"), we can replicate their exact split-sample analysis rather than
# reconstruct it from summary statistics alone (which isn't fully possible
# from r values without also having group-wise means/SDs, which their paper
# doesn't report).
#
# Reported values to validate against (their Table 1, VVIQ-TAS row):
#   Aphantasia (VVIQ 16-32, n=153):     r = 0.186
#   Non-aphantasia (VVIQ 33-80, n=680): r = -0.236
#
# Confirmed before running this: filtering all_data to study == "kvamme"
# gives exactly n=833 total, with vviq_group_2 splitting into 153/680 —
# matching their reported sample sizes exactly.
#
# Uses lm(), not brms — this is for figure annotation/reference lines, not
# inferential modelling, and matching Kvamme et al.'s own frequentist
# method exactly is the more honest comparison (they did not use Bayesian
# methods either).

kvamme_data <- all_data |> dplyr::filter(study == "kvamme")

# Split using the SAME boundary Kvamme et al. used: VVIQ 16-32 (aphantasia)
# vs. VVIQ 33-80 (non-aphantasia). Using vviq_group_2 if it's defined on
# this exact boundary (confirmed above via the 153/680 count), rather than
# re-deriving the split manually.
kvamme_aphant  <- kvamme_data |> dplyr::filter(vviq_group_2 == "aphantasia")
kvamme_typical <- kvamme_data |> dplyr::filter(vviq_group_2 != "aphantasia")

# ---------------------------------------------------------------------------- #
# Fit the two split-sample regressions ----
# ---------------------------------------------------------------------------- #
lm_kvamme_aphant  <- lm(tas ~ vviq, data = kvamme_aphant)
lm_kvamme_typical <- lm(tas ~ vviq, data = kvamme_typical)

# ---------------------------------------------------------------------------- #
# Validation against reported r ----
# VALIDATION — check recovered r against Kvamme et al.'s reported values
# BEFORE trusting these lines for any figure. If these don't closely match
# 0.186 / -0.236, investigate before proceeding (see script header for the
# two most likely causes: TAS scoring version, or a subtly different
# aphantasia/non-aphantasia boundary than intended).
# ---------------------------------------------------------------------------- #
r_aphant  <- sqrt(summary(lm_kvamme_aphant)$r.squared) * sign(coef(lm_kvamme_aphant)["vviq"])
r_typical <- sqrt(summary(lm_kvamme_typical)$r.squared) * sign(coef(lm_kvamme_typical)["vviq"])

cat("=== Validation against Kvamme et al. Table 1 ===\n")
cat(sprintf("Aphantasia (VVIQ 16-32):     recovered r = %.3f | reported r = 0.186\n", r_aphant))
cat(sprintf("Non-aphantasia (VVIQ 33-80): recovered r = %.3f | reported r = -0.236\n", r_typical))
cat(sprintf("N check: aphantasia n = %d (expected 153) | non-aphantasia n = %d (expected 680)\n",
    nrow(kvamme_aphant), nrow(kvamme_typical)))
cat("\nIf recovered r values are close to reported (within rounding/minor TAS\n")
cat("scoring differences), these lines can be captioned as Kvamme et al.'s\n")
cat("actual reported analysis. If they diverge meaningfully, investigate\n")
cat("before using these lines in any figure.\n\n")

# ---------------------------------------------------------------------------- #
# Coefficients for the figure ----
# Coefficients, for direct use in a figure (e.g. geom_abline() or a
# prediction grid matching the segmented-model figure's approach)
# ---------------------------------------------------------------------------- #
cat("=== Coefficients ===\n")
cat("Aphantasia line (VVIQ 16-32):\n")
print(coef(lm_kvamme_aphant))
cat("\nNon-aphantasia line (VVIQ 33-80):\n")
print(coef(lm_kvamme_typical))

# ---------------------------------------------------------------------------- #
# Prediction grids ----
# Prediction grids, matching the style already used in plotting_model_overlay.R
# and plot_floor_group.R (separate grids per regime, restricted to each
# regime's own real VVIQ range — no extrapolation beyond what Kvamme et al.
# themselves would have shown).
# ---------------------------------------------------------------------------- #
kvamme_aphant_grid <- data.frame(vviq = seq(16, 32, length.out = 50))
kvamme_aphant_grid$estimate <- predict(lm_kvamme_aphant, newdata = kvamme_aphant_grid)

kvamme_typical_grid <- data.frame(vviq = seq(33, 80, length.out = 50))
kvamme_typical_grid$estimate <- predict(lm_kvamme_typical, newdata = kvamme_typical_grid)

# These two grids (kvamme_aphant_grid, kvamme_typical_grid) are ready to
# feed into a geom_line() overlay on the segmented-model figure.
