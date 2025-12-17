# Plot individual participants coloured by VVIQ group

This function adds a layer to a ggplot object that plots individual data
points coloured by VVIQ group.

## Usage

``` r
plot_coloured_subjects(x, y, df = all_data, alpha = 0.4, ...)
```

## Arguments

- x:

  Variable for the x-axis.

- y:

  Variable for the y-axis.

- df:

  Data frame containing the data. Default is `all_data`.

- alpha:

  Transparency level of the points. Default is 0.4.

- ...:

  Additional arguments passed to `geom_point()`.

## Value

A list containing a `geom_point` layer for ggplot2.
