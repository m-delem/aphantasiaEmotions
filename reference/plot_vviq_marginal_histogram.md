# Plot the marginal VVIQ distribution (floor spike + continuous remainder)

Shows the empirical VVIQ distribution across the whole sample as a
histogram, with the floor bin (VVIQ=16) coloured to match
[`plot_floor_group()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_floor_group.md)'s
violin, and the remainder coloured by a viridis gradient matching that
function's scatter — visually establishing the "floor spike + continuum"
pattern that motivates the floor-group model.

## Usage

``` r
plot_vviq_marginal_histogram(
  data,
  binwidth = 1,
  x_lab = NULL,
  y_lab = "Participant count",
  vviq_breaks = seq(16, 80, 4),
  base_theme = ggplot2::theme_minimal,
  axis_relative_size = 1,
  axis_relative_y = 0.85,
  col_width_prop = 0.9,
  floor_fill_color = "#C44E52",
  floor_line_color = "#8B3A3E",
  floor_linewidth = 0.2,
  ...
)
```

## Arguments

- data:

  The data frame containing `vviq` and `complete_aphant` (must match the
  same definition used to fit floor_group_additive).

- binwidth:

  Histogram bin width, in VVIQ-scale units. Default 2.

- x_lab:

  Label for the x-axis. Omitted by default, the plot is most often meant
  to sit above the floor-group model's plot.

- y_lab:

  Label for the y-axis.

- vviq_breaks:

  x-axis breaks — MUST MATCH plot_floor_group()'s `vviq_breaks` argument
  if composing the two via patchwork, or the panels will misalign.
  Default `seq(16, 80, 4)`, matching plot_floor_group()'s own default.

- base_theme:

  A ggplot2 theme function. Default
  [`ggplot2::theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- axis_relative_size:

  Relative size of the axis text. Default is 1.

- axis_relative_y:

  Relative size of the y-axis text. Default is 0.85.

- col_width_prop:

  Proportion of `binwidth` used as each column's plotted width (leaves a
  small gap between columns). Default 0.9.

- floor_fill_color:

  Fill colour for the floor bin (VVIQ=16). Default is "#C44E52" —
  matches
  [`plot_floor_group()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_floor_group.md)'s
  violin fill by default; keep these in sync if composing the two panels
  together.

- floor_line_color:

  Border colour for the floor bin. Default is "#8B3A3E" — matches
  [`plot_floor_group()`](https://m-delem.github.io/aphantasiaEmotions/reference/plot_floor_group.md)'s
  violin border by default.

- floor_linewidth:

  Border line width for the floor bin. Default is 0.2.

- ...:

  Additional arguments passed to
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md).

## Value

A ggplot2 object (a plain histogram, NOT a patchwork composite — compose
with plot_floor_group() yourself, e.g. via `/` from patchwork).
