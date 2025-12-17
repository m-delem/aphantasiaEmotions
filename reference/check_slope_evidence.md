# Check VVIQ slope data for Bayesian GAMs

This function checks the visualisation recipe of estimated VVIQ slopes
from a
[`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html)
object computed on a Bayesian Generalised Additive Model. The function
adds an "Evidence" column to the data, categorizing slopes as "non null"
if the probability of direction (PD) exceeds 97% CI, or "uncertain"
otherwise. This allows to check manually the thresholds to choose for
the ".f_group" column in the plotting function. This whole ordeal was
necessary because the `modelbased` package did not create the ".groups"
column correctly with `brms` GAM objects.

## Usage

``` r
check_slope_evidence(slopes, digits = 3)
```

## Arguments

- slopes:

  A
  [`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html)
  object containing estimated slopes.

## Value

A data frame with slope statistics.
