# Plot the floor-group additive model against the data

Visualises a floor-group additive model's key claim: a single continuous
relationship among above-floor participants, with the
complete-aphantasia (floor) group's actual mean shown alongside where
that relationship, extrapolated to VVIQ=16, would have predicted it —
making the model's central coefficient (the floor-group shift) a visible
gap rather than an abstract number. Works with any outcome variable
(total TAS-20 score or any of its subscales) — the outcome column is
read directly from the fitted model object, not hardcoded, so the same
function call pattern applies whether `model` was fit on `tas`,
`tas_identify`, `tas_describe`, or `tas_external`.

## Usage

``` r
plot_floor_group(
  model,
  data,
  x_lab = "VVIQ score",
  y_lab = "Total TAS score",
  limits = c(8, 81),
  vviq_breaks = seq(16, 80, 4),
  tas_breaks = seq(20, 100, 20),
  dot_size = 1.2,
  dot_alpha = 0.4,
  cross_size = 2,
  cross_stroke = 0.8,
  violin_width = 3,
  stat_txt_size = 1.75,
  xbar_label = "Sample\nmean",
  xbar_face = "italic",
  xbar_vjust = 0.5,
  colorbar_width = 14,
  mean_line_color = "grey40",
  mean_line_width = 0.2,
  floor_fill_color = "#C44E52",
  floor_line_color = "#8B3A3E",
  floor_violin_alpha = 0.6,
  floor_violin_linewidth = 0.2,
  floor_violin_nudge = -2,
  floor_jitter_alpha = 0.2,
  floor_jitter_size = 1.2,
  floor_pointrange_linewidth = 0.5,
  floor_pointrange_size = 0.4,
  floor_guide_color = "grey82",
  floor_guide_linewidth = 0.2,
  floor_guide_y_pad = 5,
  floor_label_x = 15.2,
  floor_label_y = 20,
  floor_label_color = "grey65",
  floor_label_size = 1.75,
  arrow_x = 10.5,
  arrow_linewidth = 0.3,
  arrow_length = 0.05,
  tick_linewidth = 0.2,
  stat_label_x = 8.2,
  stat_label_lineheight = 0.9,
  fitted_line_color = "black",
  fitted_line_width = 0.5,
  extrap_line_width = 0.4,
  base_theme = ggplot2::theme_minimal,
  axis_relative_size = 0.85,
  axis_relative_x = 1,
  axis_relative_y = 1,
  ...
)
```

## Arguments

- model:

  A fitted brms model from a formula of the form
  `<outcome> ~ vviq + complete_aphant` or
  `<outcome> ~ vviq + complete_aphant + (vviq | study)` — single- or
  multi-level both work, thanks to `re_formula = NA` on the internal
  prediction calls.

- data:

  The data frame the model was fit on (must contain `vviq`, the model's
  outcome variable, and `complete_aphant`).

- x_lab:

  Label for the x-axis. Default is "VVIQ score".

- y_lab:

  Label for the y-axis. Default "Total TAS score" — override this
  explicitly when plotting a subscale model (e.g. "TAS DIF score"),
  since the default is not derived from the outcome variable
  automatically.

- limits:

  Limits of the x-axis. Default is c(8, 81) to accomodate both the
  half-violin and stats on the left and the secondary axis on the right.

- vviq_breaks:

  Breaks (ticks) for the VVIQ x-axis

- tas_breaks:

  Breaks for the outcome variable (the main one being tas).

- dot_size:

  Size of the individual above-floor data points. Default is 1.2.

- dot_alpha:

  Transparency of the individual above-floor data points. Default is
  0.4.

- cross_size:

  Size of the cross at the end of the regression line. Default is 2.

- cross_stroke:

  Stroke width of the cross marker. Default is 0.8.

- violin_width:

  Half-width of the floor-group violin, in VVIQ-scale units. Default 3
  (i.e. violin extends from VVIQ=16 to VVIQ=13).

- stat_txt_size:

  Size for the floor effect text on the left.

- xbar_label:

  Right-side "mean" label. Default is "Sample mean" (with a linebreak).
  Another former option was expression(bar(x)) to display a mathematical
  X-bar symbol.

- xbar_face:

  Face of the right label. Default is "italic".

- xbar_vjust:

  Vertical adjustment for the right-side "mean" label (formerly X-bar).
  Default is 0.5.

- colorbar_width:

  Width of the colorbar in the legend in pt. Default is 14

- mean_line_color:

  Colour of the sample-mean reference line (dashed horizontal line,
  labelled via the right-side X-bar axis). Default is "grey40".

- mean_line_width:

  Line width of the sample-mean reference line. Default is 0.2.

- floor_fill_color:

  Fill colour for the floor-group violin, jittered points, and mean/CI
  point-range. Default is "#C44E52" (a muted red).

- floor_line_color:

  Line/outline colour for the floor-group violin border and mean/CI
  point-range. Default is "#8B3A3E" (a darker red).

- floor_violin_alpha:

  Fill transparency of the floor-group violin. Default is 0.6.

- floor_violin_linewidth:

  Border line width of the floor-group violin. Default is 0.2.

- floor_violin_nudge:

  Horizontal nudge applied to the floor-group violin, in VVIQ-scale
  units (negative moves it further left of VVIQ=16). Default is -2.

- floor_jitter_alpha:

  Transparency of the floor-group's individual jittered points. Default
  is 0.2.

- floor_jitter_size:

  Size of the floor-group's individual jittered points. Default is 1.2.

- floor_pointrange_linewidth:

  Line width of the floor-group's mean/CI point-range. Default is 0.5.

- floor_pointrange_size:

  Point size of the floor-group's mean/CI point-range. Default is 0.4.

- floor_guide_color:

  Colour of the dotted vertical guide line marking VVIQ=16, spanning the
  violin's range. Default is "grey82".

- floor_guide_linewidth:

  Line width of the dotted vertical guide line at VVIQ=16. Default is
  0.2.

- floor_guide_y_pad:

  Additional downward padding (in outcome-scale units) below the
  violin's own range for the vertical guide line's lower end. Default is
  5.

- floor_label_x:

  Horizontal position (VVIQ-scale) of the "Floor VVIQ" text label.
  Default is 15.2.

- floor_label_y:

  Vertical position (outcome-scale) of the "Floor VVIQ" text label.
  Default is 20.

- floor_label_color:

  Colour of the "Floor VVIQ" text label. Default is "grey65".

- floor_label_size:

  Text size of the "Floor VVIQ" text label. Default is 1.75.

- arrow_x:

  Horizontal position (VVIQ-scale) of the two-way arrow and its
  connecting tick segments, marking the gap between the cross and the
  floor-group mean. Default is 10.5.

- arrow_linewidth:

  Line width of the two-way arrow. Default is 0.3.

- arrow_length:

  Length of the two-way arrow. Default is 0.05.

- tick_linewidth:

  Line width of the two short horizontal dotted tick segments connecting
  the arrow's ends to the cross and floor-group mean. Default is 0.2.

- stat_label_x:

  Horizontal position (VVIQ-scale) of the floor-effect statistics text
  label, to the left of the arrow. Default is 8.2.

- stat_label_lineheight:

  Lineheight of the stat label (to tune the mandatory linebreak).
  Default is 0.9 (ggplot2's default).

- fitted_line_color:

  Colour of the fitted above-floor regression line (both solid and
  dashed/extrapolated segments). Default is "black".

- fitted_line_width:

  Line width of the solid, real-data-range portion of the fitted
  regression line. Default is 0.5.

- extrap_line_width:

  Line width of the dashed, extrapolated portion of the fitted
  regression line. Default is 0.4.

- base_theme:

  Base ggplot2 theme to use with
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  (default is
  [`ggplot2::theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)).

- axis_relative_size:

  A numeric value for the relative size of the axis text compared to the
  base size. The default is 0.85, which is slightly smaller than the
  base size.

- axis_relative_x:

  Relative size of the x-axis text. Default is 1.

- axis_relative_y:

  Relative size of the y-axis text. Default is 1.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaEmotions/reference/theme_pdf.md)
  function for further customization of the plot theme.

## Value

A ggplot2 object.
