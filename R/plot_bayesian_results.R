#' Plot the posterior distributions of marginal contrasts
#' 
#' @description
#' This function creates a plot of the posterior distributions of marginal
#' contrasts obtained from a Bayesian model. It visualizes the distributions
#' of the contrasts along with the Region of Practical Equivalence (ROPE)
#' indicated by dashed lines. The plot uses ggplot2 and ggdist for visualization
#' and is customizable with various parameters.
#'
#' @param contrasts A marginaleffects object obtained with
#' [marginaleffects::comparisons()] containing the contrasts to plot.
#' @param model A fitted Bayesian model object (e.g., brmsfit) used to
#' determine the ROPE range.
#' @param dot_size Size of the dots representing the medians of posterior draws. 
#' Default is 0.1.
#' @param slab_alpha Alpha transparency for the slab (density) of the posterior 
#' distributions.
#' @param scale_colours A vector of two colours for the fill scale, where the 
#' first colour represents draws within the ROPE and the second colour
#' represents draws outside the ROPE. Default uses viridis colours.
#' @param x_lab Label for the x-axis. Default is "Effect size".
#' @param base_size Base font size for the plot theme. Default is 7.
#' @param rope_txt Size of the ROPE text annotation. Default is 1.5.
#' @param ... Additional arguments passed to the [theme_pdf()] function for 
#' further customization of the plot theme.
#'
#' @returns A ggplot2 object representing the posterior contrasts plot.
#' @export
plot_posterior_contrasts <- function(
    contrasts, model, 
    dot_size = 0.1,
    slab_alpha = 0.3,
    scale_colours = c(
      viridis::viridis(100)[55],
      viridis::viridis(100)[5]
    ),
    x_lab = "Effect size",
    base_size = 7,
    rope_txt = 1.5,
    ...
) {
  rlang::check_installed("bayestestR")
  rlang::check_installed("ggdist")
  rlang::check_installed("marginaleffects")
  rlang::check_installed("viridis")
  
  p <- 
    contrasts |> 
    marginaleffects::posterior_draws() |>
    dplyr::select("contrast", "draw") |> 
    dplyr::distinct() |> 
    ggplot2::ggplot(
      ggplot2::aes(
        y = .data$contrast, 
        x = .data$draw
      )
    ) +
    ggplot2::geom_vline(
      xintercept = bayestestR::rope_range(model),
      linetype = "dashed",
      linewidth = 0.1,
      color = viridis::viridis(100)[1]
    )  +
    ggplot2::annotate(
      x = 0,
      y = 6.75,
      geom = "text",
      label = "ROPE",
      color = viridis::viridis(100)[1],
      size = rope_txt,
    ) +
    ggdist::stat_slabinterval(
      mapping = ggplot2::aes(
        fill = ggplot2::after_stat(
          abs(x) < abs(bayestestR::rope_range(model)[1])
        )
      ),
      point_size = dot_size,
      color = viridis::viridis(100)[1],
      .width = .95,
      interval_size_range = c(0.2, 0.2),
      slab_alpha = slab_alpha,
      scale = 0.70,
      na.rm = TRUE,
      show.legend = FALSE,
    ) +
    ggplot2::scale_fill_manual(values = scale_colours) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(15),
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) stringr::str_to_title(x)
    ) +
    ggplot2::labs(
      x = x_lab,
      y = NULL
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      base_size = base_size,
      axis.text.y = ggplot2::element_text(
        size = base_size - 1,
        color = "black"
      ),
      ...
    )
  
  return(p)
}

#' Plot GAM estimated means
#' 
#' @description
#' This function creates a plot of estimated means from a Generalized Additive
#' Model (GAM) using the `modelbased` package. It visualizes the
#' estimated means along with confidence intervals and allows for
#' customization of various plot elements such as titles, labels, and themes.
#'
#' @param model A fitted GAM model object.
#' @param by Variable name to estimate means by (default is "vviq").
#' @param length Number of points to estimate along the predictor variable 
#' (default is 65, the number of unique VVIQ values).
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param breaks Breaks for the y-axis (default uses pretty breaks).
#' @param base_theme Base ggplot2 theme to use with [theme_pdf()]
#' (default is `ggplot2::theme_minimal`).
#' @param base_size Base font size for the plot theme (default is 7).
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' further customization of the plot theme.
#'
#' @returns A ggplot2 object representing the GAM estimated means plot.
#' @export
plot_gam_means <- function(
    model, 
    by = "vviq",
    length = 65,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    x_lab  = "VVIQ score",
    y_lab  = "Score",
    breaks = scales::breaks_pretty(10),
    base_theme = ggplot2::theme_minimal,
    ...
) {
  rlang::check_installed("modelbased")
  
  means <- modelbased::estimate_means(model, by = by, length = length)
  
  p <- 
    plot(means) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::scale_y_continuous(breaks = breaks) +
    theme_pdf(
      base_theme = base_theme,
      ...
    )
  
  return(p)
}

#' Check VVIQ slope data for Bayesian GAMs
#' 
#' @description
#' This function checks the visualisation recipe of estimated VVIQ slopes from a
#' [modelbased::estimate_slopes()] object computed on a Bayesian Generalised
#' Additive Model. The function adds an "Evidence" column to the data,
#' categorizing slopes as "non null" if the probability of direction (PD) 
#' exceeds 97% CI, or "uncertain" otherwise. This allows to check manually the
#' thresholds to choose for the ".f_group" column in the plotting function. 
#' This whole ordeal was necessary because the `modelbased` package did not 
#' create the ".groups" column correctly with `brms` GAM objects.
#'
#' @param slopes A [modelbased::estimate_slopes()] object containing estimated
#' slopes.
#'
#' @returns A data frame with slope statistics.
#' @export
check_slope_evidence <- function(slopes, digits = 3) {
  rlang::check_installed("modelbased")
  
  recipe <- modelbased::visualisation_recipe(slopes)
  
  for (i in seq_along(recipe)) {
    if (
      !is.null(recipe[[i]]$data) && 
      all(c("CI_low", "CI_high") %in% colnames(recipe[[i]]$data))
    ) {
      data <- 
        recipe[[i]]$data |> 
        dplyr::mutate(
          Evidence = ifelse(
            pd >= 0.97, 
            "Non null",
            "Uncertain"
          ),
          dplyr::across(tidyselect::where(is.numeric), ~ round(., digits))
        ) |> 
        tidyr::unite("CI", CI_low, CI_high, sep = ", ") |>
        dplyr::mutate(CI = paste0("[", CI, "]")) |>
        dplyr::select(
          VVIQ = vviq, 
          Median, CI, 
          PD = pd, 
          `Inside ROPE` = ROPE_Percentage, 
          Evidence
        )
    }
  }
  
  return(data)
}

#' Plot GAM estimated slopes
#' 
#' @description
#' This function creates a plot of estimated slopes from a Generalized Additive
#' Model (GAM) using the `modelbased` package. It visualizes the
#' estimated slopes along with confidence intervals and categorizes them based
#' on whether their 95% credible intervals exclude zero. Note that in case 
#' Bayesian models are used (such as in this whole project), the `modelbased`
#' package does not create the ".groups" column correctly for GAM objects,
#' so the user must provide a custom grouping function via the `.f_groups`
#' argument to categorize the slopes appropriately. The
#' [check_slope_evidence()] function can be used to inspect the slope data
#' and determine suitable thresholds for categorization.
#'
#' @param slopes A [modelbased::estimate_slopes()] object containing estimated 
#' slopes.
#' @param .f_groups A function or expression to create a grouping variable for
#' categorizing slopes. This is necessary to correctly identify slopes with
#' non-null evidence when using Bayesian GAM models. The function should return
#' a grouping variable based on the slope data. Default is `1`, which means
#' all slopes are treated as a single group (it will most likely result in an
#' error if unchanged).
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param base_theme Base ggplot2 theme to use with [theme_pdf()]
#' (default is `ggplot2::theme_minimal`).
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' further customization of the plot theme.
#'
#' @returns A ggplot2 object representing the GAM estimated slopes plot.
#' @export
#'
#' @examples
#' \dontrun{
#' gam_tot <- 
#'   fit_brms_model(
#'     formula = tas ~ s(vviq), 
#'     data = all_data,
#'     prior = c(brms::prior(normal(0, 20), class = "b")),
#'     file = "models/gam_tot.rds"
#'   )
#' 
#' slopes_tot <-
#'   modelbased::estimate_slopes(
#'   gam_tot, trend = "vviq", by = "vviq", length = 75)
#' 
#' check_slope_evidence(slopes_tot)
#' plot_gam_slopes(
#'   slopes_tot,
#'   .f_groups = dplyr::case_when(
#'     vviq <= 24 ~ 1,
#'     vviq <= 45 ~ 2,
#'     vviq <= 76 ~ 3,
#'     vviq <= 80 ~ 4
#'    ),
#'    y_lab = "TAS variation per unit change in VVIQ",
#'  )
#' }
plot_gam_slopes <- function(
    slopes, 
    .f_groups = 1,
    x_lab = "VVIQ score",
    y_lab = "Slope (score variation per unit change in VVIQ)",
    base_theme = ggplot2::theme_minimal,
    ...
) {
  .f_groups <- substitute(.f_groups)
  
  recipe <- modelbased::visualisation_recipe(slopes)
  
  for (i in seq_along(recipe)) {
    if (
      !is.null(recipe[[i]]$data) && 
      all(c("CI_low", "CI_high") %in% colnames(recipe[[i]]$data))
    ) {
      recipe[[i]]$data <-
        recipe[[i]]$data |> 
        dplyr::mutate(
          evidence = ifelse(
            pd >= 0.97, 
            "non_null",
            "uncertain"
          ),
          .group = eval(.f_groups)
        )
      
      if (i == 3) recipe[[i]]$aes$color <- "evidence"
      recipe[[i]]$aes$fill  <- "evidence"
    }
  }
  
  p <- 
    plot(recipe) +
    scale_x_vviq() +
    ggplot2::scale_discrete_manual(
      aesthetics = c("color", "fill"),
      name = NULL,
      values = c(
        "uncertain" = viridis::viridis(100)[5],
        "non_null" = viridis::viridis(100)[55]
      ),
      labels = c(
        "uncertain" = "PD < 97%",
        "non_null" = "PD > 97%"
      )
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      legend.key.width = grid::unit(7, "pt"),
      ...
    )
  
  return(p)
}