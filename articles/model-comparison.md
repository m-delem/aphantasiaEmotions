# Model comparison

``` r

library(aphantasiaEmotions)
library(ggplot2)
library(patchwork)

# Models and results are loaded directly from their saved artefacts in the 
# vignette, and explicitly never refitted (this is merely to protect the
# website). The "pkg" shorthand will be used throughout to point to the files of
# the aphantasiaEmotions package.
# See the Implementation Notes page for how these models were actually built.
pkg <- "aphantasiaEmotions"
refit <- "never"
```

Given VVIQ and TAS-20 scores for 1478 participants pooled across five
studies, what is the best way to describe their relationship? This page
walks through that question as it was actually answered — not by picking
one model and reporting it, but by testing an escalating sequence of
increasingly flexible approaches against each other, and letting the
evidence decide. The answer that comes out the other end is a genuine
surprise: the model that wins is not the most sophisticated one, but the
simplest one that captures the right structure. That model, and why it
wins, is the subject of the [next
page](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html);
this one is about the comparison itself.

Every model on this page converged cleanly and passed its posterior
predictive checks — the [model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html)
page has the full detail for each one; this page links to the relevant
section as each model is introduced, rather than to establish
convergence here.

## Where this comparison came from

The original plan for this study used two models: a categorical
comparison across the four established VVIQ groups — aphantasia,
hypophantasia, typical, hyperphantasia — following Reeder et al. (2024),
and a Bayesian generalised additive model (GAM) as a continuous,
non-linear alternative. Both are still part of the comparison below.

The rest of this page exists because of peer review. One reviewer raised
three points worth stating plainly, since they shaped everything that
follows:

1.  The categorical model is, functionally, already a non-linear model —
    cutting a continuum into groups lets each group take its own mean,
    which is a more flexible move than it might look. It should not be
    treated as a “linear” baseline to contrast against the GAM.
2.  A genuinely naive linear model was missing from the comparison, and
    belonged there as the actual baseline.
3.  More generally, a single categorical model and a single GAM is not a
    *comparison* — a proper one would fit several candidate models (the
    reviewer specifically suggested spline approaches like MARS, via the
    `earth` package, and mixture regression, via `flexmix`) and compare
    them quantitatively.

Point 3 is why this page exists in its current form. The spline
suggestion led directly to the segmented model described below; the
mixture-regression suggestion is still open, and is discussed on the
[For those who come
after](https://m-delem.github.io/aphantasiaEmotions/articles/for-those-who-come-after.html)
page.

## The naive baseline

The most common approach in the aphantasia literature is a single
threshold — typically VVIQ $`\leq`$ 32 — splitting the sample into
“aphantasics” and everyone else:

``` r

lm_categorical_2g <- fit_brms_model(
  formula = tas ~ vviq_group_2,
  data    = all_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file("models", "lm_categorical_2g_tot.rds", package = pkg),
  file_refit = refit
)
```

It is a reasonable starting point, and exactly what point 2 above asked
for: a true baseline, not a straw-man. (Convergence and posterior
predictive check: [model diagnostics §Categorical, 2
groups](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-categorical-2).)

``` r

p_2g <- 
  plot_group_violins(
    tas ~ vviq_group_2,
    data = all_data,
    y_lab = "Total TAS score",
    base_size = 16
  ) +
  scale_x_aphantasia() +
  ggplot2::scale_color_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::scale_fill_manual(values = c("grey50", "grey50"), guide = "none") +
  ggplot2::labs(title = "The common approach")

p_4g <-
  plot_group_violins(
    tas ~ vviq_group_4,
    data = all_data,
    y_lab = NULL,
    base_size = 16
  ) +
  scale_x_aphantasia() +
  scale_discrete_aphantasia() +
  ggplot2::labs(title = "A finer-grained 4-group alternative")

p_2g + p_4g
```

![Two side-by-side raincloud plots comparing Total TAS score. The left
panel shows the naive 2-group split (VVIQ \<= 32 vs. above), both
violins shown in grey to signal that this comparison is largely
undifferentiated. The right panel shows the richer 4-group split
(aphantasia, hypophantasia, typical, hyperphantasia), in colour, showing
much more visible separation between
groups.](model-comparison_files/figure-html/baseline-comparison-1.png)

## A finer-grained alternative

Splitting the aphantasia range itself — separating complete aphantasia
(VVIQ = 16) from hypophantasia — visibly recovers structure the coarser
2-group split misses (right panel above):

``` r

lm_categorical_4g <- fit_brms_model(
  formula = tas ~ vviq_group_4,
  data    = all_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file("models", "lm_categorical_4g_tot.rds", package = pkg),
  file_refit = refit
)
```

This is the study’s original categorical model, following Reeder et al.
(2024)’s four-group framework, and it is genuinely informative: as the
panel shows, it differentiates the sample far more than the naive
threshold does. (Convergence and posterior predictive check: [model
diagnostics §Categorical, 4
groups](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-categorical-4).)

It is also, as the reviewer pointed out, already a non-linear model in
its own right — worth keeping in mind for what follows, since the
continuous models below are not simply “adding” non-linearity that the
categorical model lacked; they are asking whether a *different kind* of
non-linearity describes the data better.

## Continuous alternatives

Three continuous models were compared against each other and against the
categorical baseline above: a plain linear model, a Bayesian GAM (the
study’s original planned non-linear approach), and a segmented
(piecewise) model with an estimated breakpoint (the reviewer’s
suggestion).

``` r

lm_linear <- fit_brms_model(
  formula = tas ~ vviq,
  data    = all_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file("models", "lm_linear_tot.rds", package = pkg),
  file_refit = refit
)

gam_tot <- fit_brms_model(
  formula = tas ~ s(vviq),
  data    = all_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file("models", "gam_tot.rds", package = pkg),
  file_refit = refit
)
```

The segmented model is the direct methodological response to the spline
suggestion above. Its breakpoint was first located with
`earth::earth(tas ~ vviq, data = all_data)`, a fast, frequentist spline
search — which found a single knot at VVIQ = 24. That value then became
the starting point (not the final answer) for a proper Bayesian
estimate: a non-linear model that estimates the breakpoint itself, with
full posterior uncertainty, rather than treating `earth`’s point
estimate as fixed:

``` r

segmented_estimated <- fit_brms_model(
  formula = brms::bf(
    tas ~ a + b1 * vviq + b2 * (vviq - k) * step(vviq - k),
    a ~ 1, b1 ~ 1, b2 ~ 1, k ~ 1,
    nl = TRUE
  ),
  data  = all_data,
  prior = c(
    brms::prior(normal(0, 20), nlpar = "a"),
    brms::prior(normal(0, 20), nlpar = "b1"),
    brms::prior(normal(0, 20), nlpar = "b2"),
    brms::prior(normal(24, 10), nlpar = "k")
  ),
  file = system.file(
    "models", "segmented_estimated_knot_tot.rds", package = pkg),
  file_refit = refit
)
```

Getting this specific model to fit correctly was not entirely
straightforward — see the [Implementation
Notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html#the-segmented-models-estimated-knot)
page for the two real problems this formula ran into and how they were
resolved.

``` r

pred_grid <- data.frame(vviq = seq(16, 80, length.out = 200))

pred_linear    <- as.data.frame(marginaleffects::predictions(lm_linear, newdata = pred_grid))
pred_gam       <- as.data.frame(marginaleffects::predictions(gam_tot, newdata = pred_grid))
pred_segmented <- as.data.frame(marginaleffects::predictions(segmented_estimated, newdata = pred_grid))

pred_linear$model    <- "Linear"
pred_gam$model       <- "GAM"
pred_segmented$model <- "Segmented"

all_preds <- rbind(
  pred_linear[, c("vviq", "estimate", "model")],
  pred_gam[, c("vviq", "estimate", "model")],
  pred_segmented[, c("vviq", "estimate", "model")]
)

model_colors <- c("Linear" = "grey40", "GAM" = "#E69F00", "Segmented" = "#009E73")

ggplot2::ggplot(all_preds, ggplot2::aes(x = vviq, y = estimate, color = model)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(x = "VVIQ score", y = "Total TAS score", color = NULL) +
  theme_pdf(base_size = 16)
```

![A line plot comparing three fitted models of Total TAS score as a
function of VVIQ score: a linear model (grey, monotonic decline), a GAM
(yellow, smooth curve peaking around VVIQ 25-30), and a segmented model
(green, sharp rise then decline, peaking near VVIQ 20). All three curves
converge in the middle of the VVIQ range and diverge at the
extremes.](model-comparison_files/figure-html/model-overlay-1.png)

### Comparing all single-level models

Convergence and posterior predictive checks for the three continuous
models above:
[linear](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-linear),
[GAM](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-gam),
[segmented, estimated
knot](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html#ppc-segmented-estimated).

``` r

comparison_table <- readRDS(
  system.file("results", "comparison_all_models_tot.rds", package = pkg)
)
comparison_table |> knitr::kable(digits = 2)
```

| model                | elpd_diff | se_diff | n_high_pareto_k |
|:---------------------|----------:|--------:|----------------:|
| segmented_fixed      |      0.00 |    0.00 |               0 |
| segmented_estimated  |     -0.09 |    1.98 |               0 |
| floor_group_additive |     -1.94 |    3.23 |               0 |
| gam                  |     -4.62 |    2.34 |               0 |
| linear               |    -22.36 |    7.55 |               0 |
| categorical_4_groups |    -29.75 |    8.17 |               0 |
| categorical_2_groups |    -48.46 |   10.17 |               0 |

*(Table restricted to single-level models fit on the total TAS score.
The floor-group additive model — introduced below and covered fully on
the [next
page](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
— was also given a multilevel treatment to check its robustness across
studies; that comparison is not shown here, since it would not be a fair
comparison against models that were never given the same multilevel
treatment.)*

The categorical and 2-group models are clearly outperformed. The linear,
GAM, and segmented models cluster closely together — genuinely close
enough that no single one of them can be called definitively best on
statistical grounds alone.

## Validating against the literature: Kvamme et al.

Kvamme et al. (2026) published a related but distinct analysis of a
large, independent sample (the `kvamme` component of this project’s own
pooled data): rather than a single continuous model, they split their
sample at a fixed VVIQ = 32 threshold and fit two separate linear
regressions, one on each side.

``` r

kvamme_data    <- all_data |> dplyr::filter(study == "kvamme")
kvamme_aphant  <- kvamme_data |> dplyr::filter(vviq_group_2 == "aphantasia")
kvamme_typical <- kvamme_data |> dplyr::filter(vviq_group_2 != "aphantasia")

lm_kvamme_aphant  <- lm(tas ~ vviq, data = kvamme_aphant)
lm_kvamme_typical <- lm(tas ~ vviq, data = kvamme_typical)

r_aphant  <- sqrt(summary(lm_kvamme_aphant)$r.squared)  * sign(coef(lm_kvamme_aphant)["vviq"])
r_typical <- sqrt(summary(lm_kvamme_typical)$r.squared) * sign(coef(lm_kvamme_typical)["vviq"])
```

Refitting their own split-sample regression method directly on their own
data reproduces their published correlations closely: r = 0.186 for
their aphantasia group (they reported .186) and r = -0.236 for their
non-aphantasia group (they reported -.236). That confirms this project’s
version of their dataset matches what they analysed.

This project’s segmented model offers a direct, principled way to test
their fixed threshold empirically, rather than assuming it.

``` r

knot_draws       <- brms::as_draws_df(segmented_estimated, variable = "b_k_Intercept")
knot_median      <- round(stats::median(knot_draws$b_k_Intercept), 1)
knot_ci_low      <- round(stats::quantile(knot_draws$b_k_Intercept, 0.025), 1)
knot_ci_high     <- round(stats::quantile(knot_draws$b_k_Intercept, 0.975), 1)
prop_below_kvamme <- round(mean(knot_draws$b_k_Intercept < 32) * 100, 1)

kvamme_aphant_grid  <- data.frame(vviq = seq(16, 32, length.out = 50))
kvamme_aphant_grid$estimate <- predict(lm_kvamme_aphant, newdata = kvamme_aphant_grid)
kvamme_typical_grid <- data.frame(vviq = seq(33, 80, length.out = 50))
kvamme_typical_grid$estimate <- predict(lm_kvamme_typical, newdata = kvamme_typical_grid)

ggplot2::ggplot() +
  ggplot2::geom_line(
    data = pred_segmented,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#009E73", linewidth = 0.9
  ) +
  ggplot2::geom_line(
    data = kvamme_aphant_grid,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = 0.6, linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = kvamme_typical_grid,
    ggplot2::aes(x = vviq, y = estimate),
    color = "#377EB8", linewidth = 0.6, linetype = "dashed"
  ) +
  ggplot2::geom_vline(xintercept = 32, linetype = "dotted", color = "#377EB8") +
  ggplot2::labs(
    x = "VVIQ score", y = "Total TAS score",
    caption = sprintf(
      "Estimated knot: %s [%s, %s] vs. Kvamme et al.'s fixed threshold (32)",
      knot_median, knot_ci_low, knot_ci_high
    )
  ) +
  theme_pdf(base_size = 16)
```

![A line plot showing the segmented model's fitted curve (green) against
two dashed reference lines (blue) representing Kvamme et al.'s own
published regression lines, each restricted to their own VVIQ range. A
vertical dotted line marks Kvamme et al.'s fixed threshold at VVIQ = 32.
A caption reports the estimated knot's median and 95% credible interval,
which sits well below Kvamme et al.'s
threshold.](model-comparison_files/figure-html/kvamme-comparison-1.png)

This project’s own estimated breakpoint — found without assuming any
threshold in advance — sits at VVIQ = 19.5 (95% CI \[17.7, 24.1\]),
meaningfully below Kvamme et al.’s fixed 32: 100% of the posterior
distribution for the knot’s location falls below 32. This does not mean
their analysis was wrong — a fixed, literature-motivated threshold is a
defensible choice, and their finding replicates the same underlying
pattern this project also finds — but it does suggest that, empirically,
the actual point where the VVIQ-TAS relationship changes shape may sit
further toward the floor of the scale than the field’s current
convention assumes.

## The twist

Look again at the comparison table above. The floor-group additive model
— a plain linear relationship among everyone above VVIQ = 16, plus a
single coefficient allowing complete aphantasics (who have no VVIQ
variance of their own to fit a slope to in the first place) to have
their own mean — is not just competitive with the GAM and segmented
models. It matches them, with far fewer moving parts. See it added to
the visual comparison with the other models:

``` r

# Creating a binary column for whether a participant is in the floor-VVIQ group
# (complete aphantasia) or not
model_data <- all_data
model_data$complete_aphant <- factor(
  ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
  levels = c("above_floor", "floor")
)

# Fitting the model
floor_group_additive <- fit_brms_model(
  formula = tas ~ vviq + complete_aphant,
  data    = model_data,
  prior   = brms::prior(normal(0, 20), class = "b"),
  file    = system.file(
    "models", "floor_group_additive_tot.rds", package = pkg),
  file_refit = refit
)

# Generating predictions
pred_grid <- 
  data.frame(vviq = seq(16, 80, length.out = 200)) |> 
  dplyr::mutate(
  complete_aphant = 
    ifelse(.data$vviq == 16, "floor", "above_floor") |> 
    factor(levels = c("above_floor", "floor"))
  )

pred_floor  <- as.data.frame(
  marginaleffects::predictions(floor_group_additive, newdata = pred_grid))

pred_floor$model <- "Floor-group"

# Combining with the predictions from the other models
all_preds <- rbind(
  pred_linear[, c("vviq", "estimate", "model")],
  pred_gam[, c("vviq", "estimate", "model")],
  pred_segmented[, c("vviq", "estimate", "model")],
  pred_floor[, c("vviq", "estimate", "model")]
)

model_colors <- c(
  "Linear" = "grey40", 
  "GAM" = "#E69F00", 
  "Segmented" = "#009E73",
  "Floor-group" = "#8B3A3E")

ggplot2::ggplot(all_preds, ggplot2::aes(x = vviq, y = estimate, color = model)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_color_manual(values = model_colors) +
  ggplot2::labs(x = "VVIQ score", y = "Total TAS score", color = NULL) +
  theme_pdf(base_size = 16)
```

![A line plot comparing four fitted models of Total TAS score as a
function of VVIQ score: a linear model (grey, monotonic decline), a GAM
(yellow, smooth curve peaking around VVIQ 25-30), a segmented model
(green, sharp rise then decline, peaking near VVIQ 20) and a model with
a separate intercept for aphantasics ('floor-group' model). All four
curves converge in the middle of the VVIQ range and diverge at the
extremes.](model-comparison_files/figure-html/model-overlay-2-1.png)

That is the actual finding this project ended up making: not that the
VVIQ-TAS relationship is curved, but that it is a straight line with one
group sitting apart from it. The [next
page](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
covers that model — and why the simplicity is the point — in full,
including how it holds up once study-level heterogeneity is accounted
for.

------------------------------------------------------------------------

**Continuing through the Extended Online Report:** this page follows the
[sample
description](https://m-delem.github.io/aphantasiaEmotions/articles/sample-description.html).
To keep reading in order, continue to [the floor-group model, in
depth](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
next. Or jump to [model
diagnostics](https://m-delem.github.io/aphantasiaEmotions/articles/model-diagnostics.html),
[implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html),
or [for those who come
after](https://m-delem.github.io/aphantasiaEmotions/articles/for-those-who-come-after.html).

------------------------------------------------------------------------

### References

Kvamme, T. L., Monzel, M., Nagai, Y., & Silvanto, J. (2026). When weak
imagery is worse than none: Core aphantasia and hypophantasia relate
differently to mental health, mediated by subjective interoception.
*Neuropsychologia*, *222*, 109368.
<https://doi.org/10.1016/j.neuropsychologia.2026.109368>

Reeder, R. R., Pounder, Z., Figueroa, A., Jüllig, A., & Azañón, E.
(2024). Non-visual spatial strategies are effective for maintaining
precise information in visual working memory. *Cognition*, *251*,
105907. <https://doi.org/10.1016/j.cognition.2024.105907>
