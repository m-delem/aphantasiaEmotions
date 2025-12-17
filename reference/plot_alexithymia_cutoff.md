# Plot Alexithymia cut-off line

This function adds a horizontal line to a ggplot object indicating the
clinical Alexithymia cut-off score, along with a text annotation.

## Usage

``` r
plot_alexithymia_cutoff(
  color = "red",
  linetype = "dashed",
  linewidth = 0.3,
  txt_x = 30,
  txt_y = 63,
  txt_size = 1.75,
  label = "Alexithymia cut-off",
  ...
)
```

## Arguments

- color:

  Color of the line and text. Default is "red".

- linetype:

  Type of the line. Default is "dashed".

- linewidth:

  Width of the line. Default is 0.3.

- txt_x:

  X-coordinate for the text annotation. Default is 30.

- txt_y:

  Y-coordinate for the text annotation. Default is 63.

- txt_size:

  Size of the text annotation. Default is 1.75.

- label:

  Text label for the annotation. Default is "Alexithymia cut-off".

- ...:

  Additional arguments passed to
  [`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html).

## Value

A list containing a horizontal line and text annotation for ggplot2.
