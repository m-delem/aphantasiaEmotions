# Custom discrete scale for Aphantasia groups

This function creates a custom discrete colour scale for Aphantasia
groups in ggplot2 plots.

## Usage

``` r
scale_discrete_aphantasia(aesthetics = c("color", "fill"), name = NULL, ...)
```

## Arguments

- aesthetics:

  Aesthetics to apply the scale to. Default is `c("color", "fill")`.

- name:

  Name of the scale. Default is `NULL`.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_discrete_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

## Value

A ggplot2 scale object for discrete aesthetics.
