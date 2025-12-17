# Custom x-axis scale for VVIQ scores

This function creates a custom x-axis scale for VVIQ scores in ggplot2
plots.

## Usage

``` r
scale_x_vviq(
  breaks = seq(16, 80, by = 4),
  expand = ggplot2::expansion(mult = c(0.02, 0.02)),
  ...
)
```

## Arguments

- breaks:

  Breaks for the x-axis. Default is `seq(16, 80, by = 4)` (which covers
  the full range of VVIQ scores).

- expand:

  Expansion for the x-axis. Default is
  `ggplot2::expansion(mult = c(0.02, 0.02))`.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

## Value

A list containing a ggplot2 scale object for the x-axis.
