# Model diagnostics

``` r

library(aphantasiaEmotions)
library(performance)
```

This page exists so that the narrative pages don’t have to report
repetitive (but still necessary) technical checks of model quality.
Every model discussed in this report converged cleanly and was checked
for the usual things a Bayesian model needs checking for: this page is
where that checking actually happens, in full, for every single-level
model at once. If you’ve arrived here from another page wanting to
confirm a specific model’s diagnostics, use the section links below
rather than reading top to bottom.

``` r

model_files <- c(
  linear                  = "lm_linear_tot.rds",
  categorical_4_groups    = "lm_categorical_4g_tot.rds",
  categorical_2_groups    = "lm_categorical_2g_tot.rds",
  gam                     = "gam_tot.rds",
  segmented_estimated     = "segmented_estimated_knot_tot.rds",
  floor_group_additive    = "floor_group_additive_tot.rds"
)

models <- lapply(model_files, function(f) {
  readRDS(system.file("models", f, package = "aphantasiaEmotions"))
})

pp_checks <- readRDS(
  system.file("results", "pp_checks_all_models.rds", package = "aphantasiaEmotions")
)
```

## Convergence

Rhat close to 1 and a healthy effective sample size (ESS) indicate that
a model’s chains mixed well and its posterior can be trusted. The table
below is computed directly from each model’s fitted object.

``` r

convergence_summary <- do.call(rbind, lapply(names(models), function(nm) {
  m <- models[[nm]]
  rhats <- brms::rhat(m)
  ess_bulk <- brms::neff_ratio(m) * brms::ndraws(m)
  data.frame(
    model    = nm,
    max_rhat = round(max(rhats, na.rm = TRUE), 4),
    min_ess  = round(min(ess_bulk, na.rm = TRUE), 0)
  )
}))

convergence_summary |> knitr::kable(row.names = FALSE)
```

| model                | max_rhat | min_ess |
|:---------------------|---------:|--------:|
| linear               |   1.0007 |    5929 |
| categorical_4_groups |   1.0013 |    5464 |
| categorical_2_groups |   1.0004 |    5218 |
| gam                  |   1.0016 |    3438 |
| segmented_estimated  |   1.0021 |    2088 |
| floor_group_additive |   1.0008 |    5639 |

Every model here has a maximum Rhat well under the conventional 1.01
threshold, and a minimum bulk ESS well above the conventional floor of
400: none of the models discussed in this report needed to be excluded
or re-fit on convergence grounds alone.

## Posterior predictive checks

A posterior predictive check compares data simulated from a fitted model
against the real, observed data. If a model’s family and structure are
reasonable, simulated and observed distributions should look similar.

### Linear

``` r

plot(pp_checks$linear) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the linear model: density curves of
simulated TAS scores overlaid on the observed TAS score
distribution.](model-diagnostics_files/figure-html/ppc-linear-1.png)

### Categorical, 4 groups

``` r

plot(pp_checks$categorical_4_groups) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the 4-group categorical model: density
curves of simulated TAS scores overlaid on the observed
distribution.](model-diagnostics_files/figure-html/ppc-categorical-4-1.png)

### Categorical, 2 groups

``` r

plot(pp_checks$categorical_2_groups) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the 2-group categorical model: density
curves of simulated TAS scores overlaid on the observed
distribution.](model-diagnostics_files/figure-html/ppc-categorical-2-1.png)

### GAM

``` r

plot(pp_checks$gam) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the GAM: density curves of simulated
TAS scores overlaid on the observed
distribution.](model-diagnostics_files/figure-html/ppc-gam-1.png)

### Segmented, estimated knot

``` r

plot(pp_checks$segmented_estimated) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the estimated-knot segmented model:
density curves of simulated TAS scores overlaid on the observed
distribution.](model-diagnostics_files/figure-html/ppc-segmented-estimated-1.png)

### Floor-group additive

``` r

plot(pp_checks$floor_group_additive) + theme_pdf(base_size = 16)
```

![Posterior predictive check for the floor-group additive model: density
curves of simulated TAS scores overlaid on the observed
distribution.](model-diagnostics_files/figure-html/ppc-floor-group-1.png)

All seven models reproduce the observed distribution well: no model
shows the kind of systematic mismatch that would call its family or
structure into question.

## Why a Gaussian family

Every model in this report uses
[`gaussian()`](https://rdrr.io/r/stats/family.html), brms’s default
family. That choice is worth checking, not just assuming: TAS-20 total
is technically a bounded, discrete sum (20-100), and a Gaussian
approximation is only appropriate if the data don’t show serious skew,
heteroscedasticity, or a pile-up at the boundaries.

``` r

tas_vals <- all_data$tas

tas_skew <- mean((tas_vals - mean(tas_vals))^3) / sd(tas_vals)^3
tas_at_boundary <- any(tas_vals <= 20 | tas_vals >= 100)

resid_skew <- sapply(models, function(m) {
  r <- residuals(m)[, "Estimate"]
  mean((r - mean(r))^3) / sd(r)^3
})

heteroscedasticity <- sapply(models, function(m) {
  f <- fitted(m)[, "Estimate"]
  r <- residuals(m)[, "Estimate"]
  cor(abs(r), f)
})

family_check_table <- data.frame(
  model = names(models),
  residual_skewness = round(resid_skew, 3),
  heteroscedasticity_r = round(heteroscedasticity, 3)
)
```

The raw TAS-20 total ranges from 20 to 94, with a skewness of 0.21 (near
0 is symmetric, this is mild). Some participants do sit exactly at the
scale’s floor (20).

``` r

family_check_table |> knitr::kable(row.names = FALSE)
```

| model                | residual_skewness | heteroscedasticity_r |
|:---------------------|------------------:|---------------------:|
| linear               |             0.139 |                0.121 |
| categorical_4_groups |             0.170 |                0.054 |
| categorical_2_groups |             0.152 |                0.108 |
| gam                  |             0.174 |                0.065 |
| segmented_estimated  |             0.173 |                0.060 |
| floor_group_additive |             0.171 |                0.063 |

Residual skewness stays small and heteroscedasticity (the correlation
between absolute residuals and fitted values) stays close to zero across
every model: nothing here would have motivated switching away from a
Gaussian family.

------------------------------------------------------------------------

**Continuing through the Extended Online Report:** this page is a
technical reference, linked to from the narrative pages rather than
meant to be read start to finish. Return to the [model
comparison](https://m-delem.github.io/aphantasiaEmotions/articles/model-comparison.html)
or [floor-group
model](https://m-delem.github.io/aphantasiaEmotions/articles/floor-group-model.html)
pages, or continue to [implementation
notes](https://m-delem.github.io/aphantasiaEmotions/articles/implementation-notes.html).
