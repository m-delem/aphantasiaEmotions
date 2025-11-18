#' Write information about the sample on the plots
#' 
#' @description 
#' This function formats the number of participants in the sample or in each 
#' group inside a call object that can be displayed nicely on ggplot2 figures.
#' It started as a personal fancy to learn how to put text in subscript on
#' ggplots with plotmath, but it ended up being pretty useful.
#'
#' @param df       A dataframe with id and Group columns
#' @param n_groups The number of groups to display. Must be either 0, 2 or 4.
#' @param type  A string indicating the type of information to display. Can be
#' either "groups" or "clusters".
#' @param prefix   A string. Text to add before the number of participants
#' @param suffix   A string. Text to add after the number of participants
#' @param ...      Other arguments passed to the function. Not used.
#'
#' @returns A call with the number of participants in each group that can be 
#' used in ggplot2 text elements (such as the title or subtitle).
#' @export
write_sample_info <- function(
    df, 
    n_groups = 2, 
    prefix = "",
    suffix = "",
    ...
) {
  if (n_groups == 0) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Total] == 
          .(nrow(df))*
          .(suffix)
      )
  } else if (n_groups == 2) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, vviq_group_2 == "aphantasia"))) ~
          ", N"[Typical.] == .(nrow(dplyr::filter(df, vviq_group_2 == "typical")))*
          .(suffix)
      )
  } else if (n_groups == 3) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, vviq_group_3 == "aphantasia"))) ~
          ", N"[Hypophant.] == 
          .(nrow(dplyr::filter(df, vviq_group_3 == "hypophantasia"))) ~
          ", N"[Typical.] == .(nrow(dplyr::filter(df, vviq_group_3 == "typical")))*
          .(suffix)
      )
  } else if (n_groups == 4) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, vviq_group_4 == "aphantasia"))) ~
          ", N"[Hypophant.] == 
          .(nrow(dplyr::filter(df, vviq_group_4 == "hypophantasia"))) ~
          ", N"[Typical.] == 
          .(nrow(dplyr::filter(df, vviq_group_4 == "typical"))) ~
          ", N"[Hyperphant.] == 
          .(nrow(dplyr::filter(df, vviq_group_4 == "hyperphantasia")))*
          .(suffix)
      )
  } else stop(glue::glue_col(
    "n_groups must be either '{blue 0}', '{cyan 2}', '{blue 3}' or '{green 4}' "
  ))
  
  return(txt)
}

scale_x_aphantasia <- function(
    name = NULL,
    mult = 0,
    add = c(0, 0.7),
    ...
) {
  scale <-
    ggplot2::scale_x_discrete(
      name = name,
      labels = c(
        "aphantasia"     = "Aphantasia",
        "hypophantasia"  = "Hypophantasia",
        "typical"        = "Typical",
        "hyperphantasia" = "Hyperphantasia"
      ),
      expand = ggplot2::expansion(
        mult = mult,
        add  = add
      ),
      ...
    )
}

scale_discrete_aphantasia <- function(
    aesthetics = c("color", "fill"),
    name = NULL,
    ...
) {
  scale <-
    ggplot2::scale_discrete_manual(
      aesthetics = aesthetics,
      name = name,
      values = c(
        "aphantasia"     = palette.colors()[1],
        "hypophantasia"  = palette.colors()[2],
        "typical"        = palette.colors()[3],
        "hyperphantasia" = palette.colors()[4]
      ),
      labels = c(
        "aphantasia"     = "Aphantasia",
        "hypophantasia"  = "Hypophantasia",
        "typical"        = "Typical",
        "hyperphantasia" = "Hyperphantasia"
      ),
      ...
    )
  
  return(scale)
}

scale_x_vviq <- function(
    breaks = seq(16, 80, by = 4),
    expand = ggplot2::expansion(mult = c(0.02, 0.02)),
    ...
) {
  scale <-
    list(
      ggplot2::scale_x_continuous(
        breaks = breaks,
        expand = expand,
        ...
      )
    )
  
  return(scale)
}

plot_coloured_subjects <- function(
    x, y, 
    df = all_data, 
    alpha = 0.4,
    ...
) {
  coloured_subjects <-
    list(
      ggplot2::geom_point(
        data = data.frame(
          x = x,
          y = y,
          color = df$vviq_group_4
        ),
        ggplot2::aes(x = x, y = y, color = color),
        alpha = alpha,
        ...
      )
    )
  
  return(coloured_subjects)
}

plot_alexithymia_cutoff <- function(
    color = "red",
    linetype = "dashed",
    linewidth = 0.3,
    txt_x = 30,
    txt_y = 63,
    txt_size = 1.75,
    label = "Alexithymia cut-off",
    ...
) {
  alexithymia_cutoff <-
    list(
      ggplot2::geom_hline(
        yintercept = 61,
        linetype = linetype,
        linewidth = linewidth,
        color = color,
        show.legend = FALSE
      ),
      ggplot2::annotate(
        "text",
        x = txt_x,
        y = txt_y,
        label = label,
        color = color,
        size = txt_size,
        hjust = 0,
        ...
      )
    )
  
  return(alexithymia_cutoff)
}

check_slope_evidence <- function(slopes) {
  
  recipe <- modelbased::visualisation_recipe(slopes)
  
  for (i in seq_along(recipe)) {
    if (
      !is.null(recipe[[i]]$data) && 
      all(c("CI_low", "CI_high") %in% colnames(recipe[[i]]$data))
    ) {
      data <- 
        recipe[[i]]$data |> 
        dplyr::mutate(
          evidence = ifelse(
            (CI_low > 0 & CI_high > 0) | (CI_low < 0 & CI_high < 0), 
            "non_null",
            "uncertain"
          )
        ) |> 
        dplyr::select(vviq, CI_low, CI_high, evidence)
    }
  }
  
  return(data)
}

plot_brm_slopes <- function(
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
            (CI_low > 0 & CI_high > 0) | (CI_low < 0 & CI_high < 0), 
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
        "uncertain" = "95% CI includes 0",
        "non_null" = "95% CI excludes 0"
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