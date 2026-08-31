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

------------------------------------------------------------------------

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.6.1 (2026-06-24)
    #>  os       Ubuntu 22.04.5 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  C.UTF-8
    #>  ctype    C.UTF-8
    #>  tz       UTC
    #>  date     2026-08-31
    #>  pandoc   3.8.3 @ /opt/hostedtoolcache/pandoc/3.8.3/x64/ (via rmarkdown)
    #>  quarto   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package            * version  date (UTC) lib source
    #>    abind                1.4-8    2024-09-12 [1] RSPM
    #>    aphantasiaEmotions * 1.0      2026-08-31 [1] local
    #>    backports            1.5.1    2026-04-03 [1] RSPM
    #>    bayesplot            1.16.0   2026-08-25 [1] RSPM
    #>    bridgesampling       1.2-1    2025-11-19 [1] RSPM
    #>    brms                 2.23.0   2025-09-09 [1] RSPM
    #>    Brobdingnag          1.2-9    2022-10-19 [1] RSPM
    #>  P bslib                0.12.0   2026-08-04 [?] RSPM
    #>  P cachem               1.1.0    2024-05-16 [?] RSPM
    #>    checkmate            2.3.4    2026-02-03 [1] RSPM
    #>  P cli                  3.6.6    2026-04-09 [?] RSPM
    #>    coda                 0.19-4.1 2024-01-31 [1] RSPM
    #>  P codetools            0.2-20   2024-03-31 [?] CRAN (R 4.6.1)
    #>  P crayon               1.5.3    2024-06-20 [?] RSPM
    #>  P curl                 8.0.0    2026-08-25 [?] RSPM
    #>  P desc                 1.4.3    2023-12-10 [?] RSPM
    #>  P devtools           * 2.5.2    2026-04-30 [?] RSPM
    #>  P digest               0.6.39   2025-11-19 [?] RSPM
    #>    distributional       0.8.1    2026-06-27 [1] RSPM
    #>    dplyr                1.2.1    2026-04-03 [1] RSPM
    #>  P ellipsis             0.3.3    2026-04-04 [?] RSPM
    #>  P evaluate             1.0.5    2025-08-27 [?] RSPM
    #>    farver               2.1.2    2024-05-13 [1] RSPM
    #>  P fastmap              1.2.0    2024-05-15 [?] RSPM
    #>  P fs                   2.1.0    2026-04-18 [?] RSPM
    #>    generics             0.1.4    2025-05-09 [1] RSPM
    #>    ggplot2              4.0.3    2026-04-22 [1] RSPM
    #>  P glue                 1.8.1    2026-04-17 [?] RSPM
    #>    gridExtra            2.3.1    2026-06-25 [1] RSPM
    #>    gtable               0.3.6    2024-10-25 [1] RSPM
    #>  P htmltools            0.5.9    2025-12-04 [?] RSPM
    #>  P htmlwidgets          1.6.4    2023-12-06 [?] RSPM
    #>    inline               0.3.21   2025-01-09 [1] RSPM
    #>    insight              1.5.3    2026-08-25 [1] RSPM
    #>  P jquerylib            0.1.4    2021-04-26 [?] RSPM
    #>  P jsonlite             2.0.0    2025-03-27 [?] RSPM
    #>  P knitr                1.51     2025-12-20 [?] RSPM
    #>    labeling             0.4.3    2023-08-29 [1] RSPM
    #>  P lattice              0.22-9   2026-02-09 [?] CRAN (R 4.6.1)
    #>  P lifecycle            1.0.5    2026-01-08 [?] RSPM
    #>    loo                  2.10.1   2026-07-24 [1] RSPM
    #>  P magrittr             2.0.5    2026-04-04 [?] RSPM
    #>  P Matrix               1.7-5    2026-03-21 [?] CRAN (R 4.6.1)
    #>    matrixStats          1.5.0    2025-01-07 [1] RSPM
    #>  P memoise              2.0.1    2021-11-26 [?] RSPM
    #>  P mgcv                 1.9-4    2025-11-07 [?] CRAN (R 4.6.1)
    #>    mvtnorm              1.4-2    2026-07-12 [1] RSPM
    #>  P nlme                 3.1-169  2026-03-27 [?] CRAN (R 4.6.1)
    #>  P otel                 0.2.0    2025-08-29 [?] RSPM
    #>    performance        * 0.18.0   2026-08-28 [1] RSPM
    #>  P pillar               1.11.1   2025-09-17 [?] RSPM
    #>  P pkgbuild             1.4.8    2025-05-26 [?] RSPM
    #>  P pkgconfig            2.0.3    2019-09-22 [?] RSPM
    #>  P pkgdown              2.2.1    2026-07-07 [?] RSPM
    #>  P pkgload              1.5.3    2026-06-15 [?] RSPM
    #>    posterior            1.7.0    2026-04-01 [1] RSPM
    #>  P purrr                1.2.2    2026-04-10 [?] RSPM
    #>    QuickJSR             1.11.0   2026-08-21 [1] RSPM
    #>  P R6                   2.6.1    2025-02-15 [?] RSPM
    #>  P ragg                 1.5.2    2026-03-23 [?] RSPM
    #>    RColorBrewer         1.1-3    2022-04-03 [1] RSPM
    #>  P Rcpp                 1.1.2    2026-07-05 [?] RSPM
    #>    RcppParallel         6.2.1    2026-08-27 [1] RSPM
    #>    renv                 1.1.4    2025-03-20 [1] RSPM (R 4.6.1)
    #>  P rlang                1.3.0    2026-07-05 [?] RSPM
    #>  P rmarkdown            2.31     2026-03-26 [?] RSPM
    #>    rstan                2.32.7   2025-03-10 [1] RSPM
    #>    rstantools           2.7.1    2026-08-29 [1] RSPM
    #>    S7                   0.2.2    2026-04-22 [1] RSPM
    #>  P sass                 0.4.10   2025-04-11 [?] RSPM
    #>    scales               1.4.0    2025-04-24 [1] RSPM
    #>    see                  0.14.1   2026-06-29 [1] RSPM
    #>  P sessioninfo          1.2.4    2026-06-04 [?] RSPM
    #>    showtext             0.9-8    2026-03-21 [1] RSPM
    #>    showtextdb           3.0      2020-06-04 [1] RSPM
    #>    StanHeaders          2.32.10  2024-07-15 [1] RSPM
    #>  P stringi              1.8.9    2026-08-04 [?] RSPM
    #>    stringr              1.6.0    2025-11-04 [1] RSPM
    #>    sysfonts             0.8.9    2024-03-02 [1] RSPM
    #>  P systemfonts          1.3.2    2026-03-05 [?] RSPM
    #>    tensorA              0.36.2.1 2023-12-13 [1] RSPM
    #>  P textshaping          1.0.5    2026-03-06 [?] RSPM
    #>  P tibble               3.3.1    2026-01-11 [?] RSPM
    #>    tidyselect           1.2.1    2024-03-11 [1] RSPM
    #>  P usethis            * 3.2.1    2025-09-06 [?] RSPM
    #>  P vctrs                0.7.3    2026-04-11 [?] RSPM
    #>  P withr                3.0.3    2026-06-19 [?] RSPM
    #>  P xfun                 0.60     2026-07-09 [?] RSPM
    #>  P yaml                 2.3.12   2025-12-10 [?] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaEmotions-8f3b5e1f/linux-ubuntu-jammy/R-4.6/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-jammy/R-4.6/x86_64-pc-linux-gnu/e7c0fad7
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
