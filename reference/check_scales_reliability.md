# Check the internal reliability of the VVIQ, TAS-20 and its sub-scales

Computes Cronbach's alpha and McDonald's omega for the VVIQ and the
TAS-20 (total score and its three subscales - DIF, DDF, EOT), separately
for each group defined by `...` (e.g. per study). Expects `data` to
contain a list-column named `items`, itself holding, for each group, a
data frame of item-level responses with columns named `vviq_q*` and
`tas_q*`.

## Usage

``` r
check_scales_reliability(
  data,
  ...,
  scales = c("vviq", "tas", "dif", "ddf", "eot"),
  digits = 2,
  silence = FALSE
)
```

## Arguments

- data:

  A data frame with one row per group and a list-column named `items`,
  where each element is a data frame of item-level responses (columns
  `vviq_q1`...`vviq_q16`, `tas_q1`...`tas_q20`).

- ...:

  Grouping variables (e.g. `study`), used to compute reliability
  separately within each group.

- scales:

  String vector with the names of the scales to examine. Has to be one
  or several among the defaults: "vviq", "tas", "dif", "ddf" and "eot".

- digits:

  Number of decimal places to round the reliability coefficients to.
  Default is 2.

- silence:

  Logical. If `TRUE`, suppresses the messages and warnings commonly
  emitted by
  [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html) and
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) (e.g.
  about reversed items or factor structure). Default is `FALSE`.

## Value

A tibble with one row per group per scale, and columns `Scale`,
`Cronbach's alpha`, `McDonald's omega`, plus the grouping variables
passed via `...`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_scales_reliability(all_data, study)
check_scales_reliability(all_data, study, digits = 3, silence = TRUE)
} # }
```
