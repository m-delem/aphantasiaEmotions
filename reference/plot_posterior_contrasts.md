# Plot the posterior distributions of marginal contrasts

This function creates a plot of the posterior distributions of marginal
contrasts obtained from a Bayesian model. It visualizes the
distributions of the contrasts along with the Region of Practical
Equivalence (ROPE) indicated by dashed lines. The plot uses ggplot2 and
ggdist for visualization and is customizable with various parameters.

## Usage

``` r
plot_posterior_contrasts(
  contrasts,
  model,
  dot_size = 0.1,
  slab_alpha = 0.3,
  scale_colours = c(viridis::viridis(100)[55], viridis::viridis(100)[5]),
  x_lab = "Effect size",
  base_size = 7,
  rope_txt = 1.5,
  ...
)
```

## Arguments

- contrasts:

  A marginaleffects object obtained with
  [`marginaleffects::comparisons()`](https://marginaleffects.com/man/r/comparisons.html)
  containing the contrasts to plot.

- model:

  A fitted Bayesian model object (e.g., brmsfit) used to determine the
  ROPE range.

- dot_size:

  Size of the dots representing the medians of posterior draws. Default
  is 0.1.

- slab_alpha:

  Alpha transparency for the slab (density) of the posterior
  distributions.

- scale_colours:

  A vector of two colours for the fill scale, where the first colour
  represents draws within the ROPE and the second colour represents
  draws outside the ROPE. Default uses viridis colours.

- x_lab:

  Label for the x-axis. Default is "Effect size".

- base_size:

  Base font size for the plot theme. Default is 7.

- rope_txt:

  Size of the ROPE text annotation. Default is 1.5.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  function for further customization of the plot theme.

## Value

A ggplot2 object representing the posterior contrasts plot.
