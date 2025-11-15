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

plot_coloured_subjects <- function(
    df, x, y, 
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
    txt_size = 2,
    label = "Alexithymia cut-off"
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
        hjust = 0
      )
    )
  
  return(alexithymia_cutoff)
}

create_evidence_groups <- function(layer, color = FALSE) {
  layer$data <-
    layer$data |> 
    mutate(
      evidence = ifelse(
        (CI_low > 0 & CI_high > 0) | (CI_low < 0 & CI_high < 0), 
        "substantial",
        "insufficient"
      ),
      .group = case_when(
        vviq <= 23 ~ 1,
        vviq <= 33 ~ 2,
        vviq <= 36 ~ 3,
        vviq <= 47 ~ 4,
        vviq <= 71 ~ 5,
        vviq <= 80 ~ 6
      )
    )
  
  if (color) layer$aes$color <- "evidence"
  layer$aes$fill  <- "evidence"
  
  return(layer)
}