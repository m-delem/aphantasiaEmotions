#' Compute counts and proportions of alexithymia groups within VVIQ groups
#' 
#' @description
#' This function computes the counts and proportions of alexithymia groups
#' within specified grouping variables (e.g., VVIQ groups) for each study
#' as well as for the combined data across all studies.
#'
#' @param df Data frame containing the data with `tas_group` and grouping 
#' variables.
#' @param ... Grouping variables to summarise the data by (e.g., vviq_group_4). 
#'
#' @returns A data frame with counts and proportions of alexithymia groups 
#' within the specified grouping variables for each study and for the combined 
#' data.
#' @export
#'
#' @examples
#' summarise_aph_and_alexi(all_data, vviq_group_4) |> head()
#' summarise_aph_and_alexi(all_data, vviq_group_2) |> head()
summarise_aph_and_alexi <- function(df, ...) {
  df_summary <-
    df |> 
    dplyr::group_by(..., .data$tas_group, .data$study) |> 
    dplyr::count() |>
    dplyr::group_by(..., .data$study) |>
    dplyr::mutate(
      n_group = sum(.data$n),
      prop = .data$n / .data$n_group
    ) |> 
    dplyr::bind_rows(
      aphantasiaEmotions::all_data |> 
        dplyr::group_by(..., .data$tas_group) |> 
        dplyr::count() |>
        dplyr::group_by(...)  |>
        dplyr::mutate(
          study = "total",
          n_group = sum(.data$n),
          prop = .data$n / .data$n_group
        )
    ) |>
    dplyr::mutate(
      study = factor(
        .data$study, 
        levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
      )
    )
  
  return(df_summary)
}

#' Plot alexithymia proportions within VVIQ groups based on a summary table
#' 
#' @description
#' This function creates a bar plot showing the proportions of alexithymia
#' groups within VVIQ groups for each study as well as for the combined data
#' across all studies. The plot includes options for customizing the appearance
#' of the bars, text labels, facets, and overall theme.
#'
#' @param df_summary A summary data frame containing counts and proportions of
#' alexithymia groups within VVIQ groups, typically generated using the
#' [summarise_aph_and_alexi()] function.
#' @param var_x The variable to be plotted on the x-axis (e.g., vviq_group_4).
#' @param bar_alpha Transparency level of the bars.
#' @param bar_lw Line width of the bar borders.
#' @param prop_threshold Threshold for displaying proportion text labels on the 
#' bars.
#' @param prop_txt_size Size of the proportion text labels on the bars.
#' @param ncol Number of columns for facet wrapping.
#' @param limits Limits for the x-axis categories.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param marg Margin size around the plot.
#' @param base_size Base font size for the plot theme.
#' @param ... Additional arguments passed to the [theme_pdf()] function.
#'
#' @returns A ggplot2 object representing the bar plot of alexithymia 
#' proportions within VVIQ groups.
#' @export
#'
#' @examples
#' df_summary <- summarise_aph_and_alexi(all_data, vviq_group_4)
#' plot_alexithymia_proportions(
#'   df_summary, vviq_group_4, 
#'   base_size = 12, 
#'   prop_txt_size = 3
#'  )
plot_alexithymia_proportions <- function(
    df_summary, 
    var_x, 
    bar_alpha = 0.3,
    bar_lw = 0.1,
    prop_threshold = 0.16,
    prop_txt_size = 1.5,
    ncol = 6,
    limits = c(
      "hyperphantasia",
      "typical",
      "hypophantasia",
      "aphantasia"
    ),
    title = NULL,
    subtitle = NULL,
    x_lab = NULL,
    y_lab = "Proportion within the VVIQ group",
    marg = 8,
    base_size = 7,
    ...
) {
  p <- 
    df_summary |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ var_x }},
        y = .data$prop,
        fill = .data$tas_group,
        colour = .data$tas_group
      )
    ) +
    ggplot2::geom_bar(
      stat = "identity", 
      position = "fill",
      alpha = bar_alpha,
      linewidth = bar_lw
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          .data$prop >= prop_threshold,
          paste0(round(.data$prop * 100, 1), "%"),
          ""
      )),
      color = "black",
      size = prop_txt_size,
      position = ggplot2::position_fill(vjust = 0.5)
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$study),
      ncol = ncol,
      labeller = ggplot2::as_labeller(c(
        "burns"  = "Ale & Burns (2024)",
        "monzel" = "Monzel et al. (2024)",
        "mas"    = "Mas & Luminet (2025)",
        "ruby"   = "Ruby (2025)",
        "kvamme" = "Kvamme et al. (2026)",
        "total"  = "All studies combined"
      )),
      axes = "all_x"
    ) +
    ggplot2::scale_x_discrete(
      # expand = c(0, 0),
      limits = limits,
      labels = c(
        "aphantasia"     = "Aphantasia",
        "hypophantasia"  = "Hypophantasia",
        "typical"        = "Typical",
        "hyperphantasia" = "Hyperphantasia"
      )
    ) +
    ggplot2::scale_y_reverse(expand = c(0.025, 0)) +
    ggplot2::coord_flip() +
    ggplot2::scale_discrete_manual(
      aes = c("fill", "colour"),
      name = NULL,
      labels = c(
        # "alexithymia" = "Alexithymia (TAS > 60)",
        # "alexithymia" = expression("Alexithymia (TAS">="61)"),
        "alexithymia" = "Alexithymia (TAS \u2265 61)",
        "typical_tas" = "No Alexithymia"
      ),
      values = grDevices::palette.colors()[c(7, 6)]
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab
    ) +
    theme_pdf(
      ggplot2::theme_minimal,
      base_size = base_size,
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = base_size, 
        color = "black",
        angle = 0, hjust = 1
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(marg, marg, marg, marg),
      plot.caption = ggplot2::element_text(
        margin = ggplot2::margin(t = marg), 
        color = "grey40"
      ),
      legend.position = "bottom",
      legend.box.spacing = grid::unit(7, "pt"),
      strip.text = ggplot2::element_text(size = base_size - 1),
      ...
    )
  
  return(p)
}

#' Plot VVIQ group proportions of a specified variable across studies
#' 
#' @description
#' This function creates a bar plot showing the proportions of a each VVIQ
#' sub-group for a given classification (e.g., vviq_group_4, vviq_group_3, etc.)
#'
#' @param df Data frame containing the data with the specified variable.
#' @param var The variable to be plotted (e.g., vviq_group_4).
#' @param bar_alpha Transparency level of the bars.
#' @param bar_lw Line width of the bar borders.
#' @param prop_txt_size Size of the proportion text labels on the bars.
#' @param marg Margin size around the plot.
#' @param base_size Base font size for the plot theme.
#' @param ... Additional arguments passed to the [theme_pdf()] function.
#'
#' @returns A ggplot2 object representing the bar plot of VVIQ group proportions
#' for the specified variable across studies.
#' @export
#'
#' @examples
#' plot_vviq_group_proportions(
#'   all_data, vviq_group_4, 
#'   base_size = 12, prop_txt_size = 3
#' )
plot_vviq_group_proportions <- function(
    df, var,
    bar_alpha = 0.3,
    bar_lw = 0.1,
    prop_txt_size = 1.75,
    marg = 8,
    base_size = 7,
    ...
) {
  p <-
    df |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = 1,
        fill = {{ var }},
        colour = {{ var }}
      )
    ) +
    ggplot2::geom_bar(
      position = "fill",
      alpha = bar_alpha,
      linewidth = bar_lw
    ) +
    ggplot2::geom_text(
      data = 
        df |> 
        dplyr::group_by({{ var }}, .data$study) |>
        dplyr::count() |>
        dplyr::group_by(.data$study) |>
        dplyr::mutate(
          n_group = sum(.data$n),
          prop = .data$n / .data$n_group
        ),
      ggplot2::aes(
        y = .data$prop,
        label = ifelse(
          .data$prop >= 0,
          .data$n,
          ""
        )
      ),
      color = "black",
      size = prop_txt_size,
      position = ggplot2::position_fill(vjust = 0.5)
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$study),
      ncol = 6,
      labeller = ggplot2::as_labeller(c(
        "burns"  = "Ale & Burns (2024)\nN = 192",
        "monzel" = "Monzel et al. (2024)\nN = 105",
        "mas"    = "Mas & Luminet (2025)\nN = 123",
        "ruby"   = "Ruby (2025)\nN = 225",
        "kvamme" = "Kvamme et al. (2026)\nN = 833",
        "total"  = "All studies combined\nN = 1,478"
      )),
      axes = "all_x"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    scale_discrete_aphantasia() +
    theme_pdf(
      ggplot2::theme_minimal,
      base_size = base_size,
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(marg, marg, marg, marg),
      plot.caption = ggplot2::element_text(
        margin = ggplot2::margin(t = marg), 
        color = "grey40"
      ),
      legend.position = "left",
      legend.text = ggplot2::element_text(hjust = 1),
      legend.text.position = "left",
      strip.text = ggplot2::element_text(size = base_size - 1),
      ...
    )
  
  return(p)
}

#' Plot group violins with means and confidence intervals
#' 
#' @description
#' This function creates a ggplot object that visualises group data using
#' half-violins, individual data points, and mean estimates with confidence
#' intervals.
#'
#' @param formula A formula specifying the response and grouping variables.
#' @param data Data frame containing the data. Default is `all_data`.
#' @param dot_size Size of the individual data points. Default is 0.5.
#' @param box.linewidth Line width of the box in the crossbar. Default is 0.1.
#' @param middle.linewidth Line width of the middle line in the crossbar. 
#' Default is 0.5.
#' @param violin_width Width of the half-violins. Default is 0.7.
#' @param violin_flip Option passed to [geom_violinhalf()]'s `flip` argument. 
#' Should the half-violin plot switch directions? 
#' By default, this is FALSE and all half-violin geoms will have the flat-side 
#' on facing leftward. 
#' If flip = TRUE, then all flat-sides will face rightward. 
#' Optionally, a numeric vector can be supplied indicating which specific geoms 
#' should be flipped (matches [see::geom_violinhalf()]'s own semantics: the 
#' vector gives the 1-indexed group numbers, in level order, to flip).
#' @param violin_nudge Signed horizontal distance to move the half-violins
#' away from the group's x tick (negative moves left, positive moves right).
#' Can be a single value (applied to every group) or a vector with one signed
#' value per group (in level order). This is independent from `violin_flip`:
#' flip controls which side the violin's flat edge faces, nudge controls how
#' far and which direction it is displaced from the tick. Each group is drawn
#' as its own layer internally, so flip and nudge can be combined freely
#' without one throwing off the other.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis. Default is "Score".
#' @param breaks Breaks for the y-axis. Default is `scales::breaks_pretty(10)`.
#' @param base_theme Base theme for the plot. Default is 
#' `ggplot2::theme_minimal`.
#' @param axis_relative_size Relative size of the axis text. Default is 1.
#' @param axis_relative_x Relative size of the x-axis text. Default is 1.
#' @param axis_relative_y Relative size of the y-axis text. Default is 0.85.
#' @param ... Additional arguments passed to the [theme_pdf()] function.
#'
#' @returns A ggplot object visualising the group data.
#' @export
#'
#' @examples
#' plot_group_violins(
#'  tas ~ vviq_group_4,
#'  y_lab = "TAS Score",
#'  base_size = 12
#')
plot_group_violins <- function(
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
) {
  model  <- stats::lm(formula, data)
  p_data <- model$model
  var_1  <- colnames(model$model)[1]
  var_2  <- colnames(model$model)[2]
  
  # --- resolve per-group flip state -----------------------------------------
  # group_levels defines left-to-right x order; index position is what 
  # geom_violinhalf's own `flip` vector semantics key off of, so we reuse 
  # the same indexing here to keep flip and nudge in sync.
  group_levels <- levels(as.factor(p_data[[var_2]]))
  n_groups <- length(group_levels)
  
  is_flipped <- if (is.logical(violin_flip) && length(violin_flip) == 1) {
    rep(violin_flip, n_groups)
  } else {
    seq_len(n_groups) %in% violin_flip
  }
  
  # --- resolve per-group nudge -----------------------------------------------
  # Sign is now taken directly from what the user passes in violin_nudge --
  # NOT derived from violin_flip. An earlier version of this function forced
  # the sign from flip state, on the assumption that flip and nudge needed
  # to be kept in lockstep to avoid desync. That assumption was only true
  # back when a single layer carried a per-row nudge vector (see the
  # per-group-layer comment below) -- with one layer per group, flip and
  # nudge can never desync structurally, so forcing the sign was no longer
  # protecting against anything; it was just silently overriding the
  # caller's chosen direction. Flip controls orientation, nudge controls
  # displacement -- they are independent knobs, and both are now left
  # fully user-controlled.
  nudge_by_group <- if (length(violin_nudge) == 1) {
    rep(violin_nudge, n_groups)
  } else if (length(violin_nudge) == n_groups) {
    violin_nudge
  } else {
    stop(
      "violin_nudge must have length 1 or length equal to the number of ",
      "groups (", n_groups, "), got length ", length(violin_nudge), "."
    )
  }
  names(nudge_by_group) <- group_levels
  
  # --- build one geom_violinhalf layer PER GROUP ------------------------------
  # Why: geom_violinhalf's stat (stat_ydensity) collapses each group's raw
  # rows down to a much smaller set of density-curve points before
  # position_nudge ever runs. A single layer carrying a per-ROW nudge
  # vector therefore gets recycled against the post-stat point count, not
  # the original row count -- silently slicing each violin into
  # differently-nudged stripes (confirmed via the "size of longer object
  # is not a multiple of shorter object size" warning during testing).
  # A scalar nudge on a single-group layer sidesteps this entirely, since
  # there is nothing left to mis-recycle against.
  violin_layers <- lapply(seq_len(n_groups), function(i) {
    this_level <- group_levels[i]
    see::geom_violinhalf(
      data = p_data[p_data[[var_2]] == this_level, , drop = FALSE],
      ggplot2::aes(
        y = .data[[var_1]],
        x = .data[[var_2]],
        fill = .data[[var_2]]
      ),
      flip = is_flipped[i],
      color = "transparent",
      alpha = 0.25,
      linewidth = 0.1,
      width = violin_width,
      position = ggplot2::position_nudge(x = nudge_by_group[[this_level]])
    )
  })
  
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = p_data,
      ggplot2::aes(
        y = .data[[var_1]],
        x = .data[[var_2]],
        color = .data[[var_2]]
      ),
      size  = dot_size,
      alpha = 0.1,
      position = ggplot2::position_jitter(
        width = 0.125,
        height = 0.25    
      )
    ) +
    ggplot2::geom_crossbar(
      data = modelbased::estimate_means(model, by = var_2),
      ggplot2::aes(
        y = .data$Mean,
        x = .data[[var_2]],
        color = .data[[var_2]],
        fill = .data[[var_2]],
        ymin = .data$CI_low,
        ymax = .data$CI_high
      ),
      alpha = 0.1,
      width = 0.15,
      box.linewidth = box.linewidth,
      middle.linewidth = middle.linewidth
    ) +
    violin_layers +
    ggplot2::scale_y_continuous(breaks = breaks) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x_lab, 
      y = y_lab
    ) +
    theme_pdf(
      base_theme = base_theme,
      axis_relative_size = axis_relative_size,
      axis_relative_x = axis_relative_x,
      axis_relative_y = axis_relative_y,
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      ...
    )
  
  return(p)
}