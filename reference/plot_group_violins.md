# Plot group violins with means and confidence intervals

This function creates a ggplot object that visualises group data using
half-violins, individual data points, and mean estimates with confidence
intervals.

## Usage

``` r
plot_group_violins(
  formula,
  data = aphantasiaEmotions::all_data,
  dot_size = 0.5,
  box.linewidth = 0.1,
  middle.linewidth = 0.5,
  violin_width = 0.7,
  violin_flip = FALSE,
  violin_nudge = 0.2,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_lab = NULL,
  y_lab = "Score",
  breaks = scales::breaks_pretty(10),
  base_theme = ggplot2::theme_minimal,
  axis_relative_size = 1,
  axis_relative_x = 0.8,
  axis_relative_y = 0.85,
  ...
)
```

## Arguments

- formula:

  A formula specifying the response and grouping variables.

- data:

  Data frame containing the data. Default is `all_data`.

- dot_size:

  Size of the individual data points. Default is 0.5.

- box.linewidth:

  Line width of the box in the crossbar. Default is 0.1.

- middle.linewidth:

  Line width of the middle line in the crossbar. Default is 0.5.

- violin_width:

  Width of the half-violins. Default is 0.7.

- violin_flip:

  Option passed to
  [`see::geom_violinhalf()`](https://easystats.github.io/see/reference/geom_violinhalf.html)'s
  `flip` argument. Should the half-violin plot switch directions? By
  default, this is FALSE and all half-violin geoms will have the
  flat-side on facing leftward. If flip = TRUE, then all flat-sides will
  face rightward. Optionally, a numeric vector can be supplied
  indicating which specific geoms should be flipped (matches
  [`see::geom_violinhalf()`](https://easystats.github.io/see/reference/geom_violinhalf.html)'s
  own semantics: the vector gives the 1-indexed group numbers, in level
  order, to flip).

- violin_nudge:

  Signed horizontal distance to move the half-violins away from the
  group's x tick (negative moves left, positive moves right). Can be a
  single value (applied to every group) or a vector with one signed
  value per group (in level order). This is independent from
  `violin_flip`: flip controls which side the violin's flat edge faces,
  nudge controls how far and which direction it is displaced from the
  tick. Each group is drawn as its own layer internally, so flip and
  nudge can be combined freely without one throwing off the other.

- title:

  Title of the plot.

- subtitle:

  Subtitle of the plot.

- caption:

  Caption of the plot.

- x_lab:

  Label for the x-axis.

- y_lab:

  Label for the y-axis. Default is "Score".

- breaks:

  Breaks for the y-axis. Default is `scales::breaks_pretty(10)`.

- base_theme:

  Base theme for the plot. Default is
  [`ggplot2::theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- axis_relative_size:

  Relative size of the axis text. Default is 1.

- axis_relative_x:

  Relative size of the x-axis text. Default is 1.

- axis_relative_y:

  Relative size of the y-axis text. Default is 0.85.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  function.

## Value

A ggplot object visualising the group data.

## Examples

``` r
plot_group_violins(
 tas ~ vviq_group_4,
 y_lab = "TAS Score",
 base_size = 12
)
```
