# Plot GAM estimated means

This function creates a plot of estimated means from a Generalized
Additive Model (GAM) using the `modelbased` package. It visualizes the
estimated means along with confidence intervals and allows for
customization of various plot elements such as titles, labels, and
themes.

## Usage

``` r
plot_gam_means(
  model,
  by = "vviq",
  length = 65,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_lab = "VVIQ score",
  y_lab = "Score",
  breaks = scales::breaks_pretty(10),
  base_theme = ggplot2::theme_minimal,
  ...
)
```

## Arguments

- model:

  A fitted GAM model object.

- by:

  Variable name to estimate means by (default is "vviq").

- length:

  Number of points to estimate along the predictor variable (default is
  65, the number of unique VVIQ values).

- title:

  Title of the plot.

- subtitle:

  Subtitle of the plot.

- caption:

  Caption of the plot.

- x_lab:

  Label for the x-axis.

- y_lab:

  Label for the y-axis.

- breaks:

  Breaks for the y-axis (default uses pretty breaks).

- base_theme:

  Base ggplot2 theme to use with
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  (default is
  [`ggplot2::theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)).

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  function for further customization of the plot theme.

- base_size:

  Base font size for the plot theme (default is 7).

## Value

A ggplot2 object representing the GAM estimated means plot.
