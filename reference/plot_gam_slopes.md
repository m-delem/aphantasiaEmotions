# Plot GAM estimated slopes

This function creates a plot of estimated slopes from a Generalized
Additive Model (GAM) using the `modelbased` package. It visualizes the
estimated slopes along with confidence intervals and categorizes them
based on whether their 95% credible intervals exclude zero. Note that in
case Bayesian models are used (such as in this whole project), the
`modelbased` package does not create the ".groups" column correctly for
GAM objects, so the user must provide a custom grouping function via the
`.f_groups` argument to categorize the slopes appropriately. The
[`check_slope_evidence()`](https://m-delem.github.io/aphantasiaEmotions/reference/check_slope_evidence.md)
function can be used to inspect the slope data and determine suitable
thresholds for categorization.

## Usage

``` r
plot_gam_slopes(
  slopes,
  .f_groups = 1,
  x_lab = "VVIQ score",
  y_lab = "Slope (score variation per unit change in VVIQ)",
  base_theme = ggplot2::theme_minimal,
  ...
)
```

## Arguments

- slopes:

  A
  [`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html)
  object containing estimated slopes.

- .f_groups:

  A function or expression to create a grouping variable for
  categorizing slopes. This is necessary to correctly identify slopes
  with non-null evidence when using Bayesian GAM models. The function
  should return a grouping variable based on the slope data. Default is
  `1`, which means all slopes are treated as a single group (it will
  most likely result in an error if unchanged).

- x_lab:

  Label for the x-axis.

- y_lab:

  Label for the y-axis.

- base_theme:

  Base ggplot2 theme to use with
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  (default is
  [`ggplot2::theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)).

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  function for further customization of the plot theme.

## Value

A ggplot2 object representing the GAM estimated slopes plot.

## Examples

``` r
if (FALSE) { # \dontrun{
gam_tot <- 
  fit_brms_model(
    formula = tas ~ s(vviq), 
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file = "models/gam_tot.rds"
  )

slopes_tot <-
  modelbased::estimate_slopes(
  gam_tot, trend = "vviq", by = "vviq", length = 75)

check_slope_evidence(slopes_tot)
plot_gam_slopes(
  slopes_tot,
  .f_groups = dplyr::case_when(
    vviq <= 24 ~ 1,
    vviq <= 45 ~ 2,
    vviq <= 76 ~ 3,
    vviq <= 80 ~ 4
   ),
   y_lab = "TAS variation per unit change in VVIQ",
 )
} # }
```
