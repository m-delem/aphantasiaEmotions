# Check that a segmented model's cached knot still matches a fresh MARS estimate

The segmented `brms` model's breakpoint prior is seeded from a fast,
frequentist MARS knot search
([`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html)), then the
model is fit once and cached to disk with `file_refit = "never"` (see
[`fit_brms_model()`](https://m-delem.github.io/aphantasiaEmotions/reference/fit_brms_model.md)
and the "Continuous alternatives" section of
`vignette("model-comparison", package = "aphantasiaEmotions")`). Because
the cached model is never automatically refitted, it can silently go
stale if the underlying data changes - the site would keep reporting a
knot fit around old data without any visible sign that anything is
wrong.

## Usage

``` r
check_knot_still_matches(data, seed_knot, formula = tas ~ vviq)
```

## Arguments

- data:

  The data the live model-comparison page is using right now (e.g.
  `all_data`).

- seed_knot:

  The knot value that was used to seed the cached model's prior when it
  was originally fit - a fixed constant recorded at fit time, not read
  from the model object itself.

- formula:

  Formula passed to
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html). Default
  is `tas ~ vviq`, matching the segmented model's own formula.

## Value

Invisibly, the freshly-computed knot value, if it matches `seed_knot`.
Errors loudly otherwise.

## Details

This function is a safeguard against exactly that. It re-runs the cheap
[`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) search on
the data supplied *now*, and compares the knot it finds against
`seed_knot` - the knot value that was actually used to seed the prior
for the cached model at the time it was fit (**not** the brms model's
own posterior estimate of the knot, which is expected to differ from the
earth seed even on unchanged data, since brms re-estimates it from a
prior rather than reproducing earth's point estimate exactly). If the
two don't match exactly, the underlying data has almost certainly
changed since the model was cached, and the model should be refit.
