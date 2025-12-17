# Custom x-axis scale for Aphantasia groups

This function creates a custom x-axis scale for Aphantasia groups in
ggplot2 plots.

## Usage

``` r
scale_x_aphantasia(name = NULL, mult = 0, add = c(0, 0.7), ...)
```

## Arguments

- name:

  Name of the x-axis.

- mult:

  Multiplier for
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html).

- add:

  Additive for
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html).

- ...:

  Additional arguments passed to
  [`ggplot2::scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html).

## Value

A ggplot2 scale object for the x-axis.
