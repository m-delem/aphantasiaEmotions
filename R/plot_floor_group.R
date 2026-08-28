# ---------------------------------------------------------------------------- #
# Floor-group plot — first attempt, following existing ggplot_tools.R /
# plot_bayesian_results.R conventions
# ---------------------------------------------------------------------------- #
#
# DESIGN NOTE: plot_gam_means() uses modelbased::estimate_means() + plot()
# on the result, leaning on modelbased's own plotting method. That pattern
# doesn't fit cleanly here, because the whole point of this figure is a
# DISCONTINUITY (floor group vs. extrapolated line), which estimate_means()
# would likely just show as a continuous prediction curve, hiding the
# actual claim. So this instead builds explicit geoms from model
# predictions, following the more manual style of plot_coloured_subjects()/
# plot_alexithymia_cutoff() (composable layers via list()), rather than
# the estimate_means()+plot() shortcut.

#' Plot the floor-group additive model against the data
#'
#' @description
#' Visualises a floor-group additive model's key claim: a single
#' continuous relationship among above-floor participants, with the
#' complete-aphantasia (floor) group's actual mean shown alongside where
#' that relationship, extrapolated to VVIQ=16, would have predicted it —
#' making the model's central coefficient (the floor-group shift) a visible
#' gap rather than an abstract number. Works with any outcome variable
#' (total TAS-20 score or any of its subscales) — the outcome column is
#' read directly from the fitted model object, not hardcoded, so the same
#' function call pattern applies whether `model` was fit on `tas`,
#' `tas_identify`, `tas_describe`, or `tas_external`.
#'
#' @param model A fitted brms model from a formula of the form
#' `<outcome> ~ vviq + complete_aphant` or
#' `<outcome> ~ vviq + complete_aphant + (vviq | study)` — single- or
#' multi-level both work, thanks to `re_formula = NA` on the internal
#' prediction calls.
#' @param data The data frame the model was fit on (must contain `vviq`,
#' the model's outcome variable, and `complete_aphant`).
#' @param x_lab Label for the x-axis. Default is "VVIQ score".
#' @param y_lab Label for the y-axis. Default "Total TAS score" — override
#' this explicitly when plotting a subscale model (e.g. "TAS DIF score"),
#' since the default is not derived from the outcome variable automatically.
#' @param limits Limits of the x-axis. Default is c(8, 81) to accomodate both
#' the half-violin and stats on the left and the secondary axis on the right.
#' @param violin_width Half-width of the floor-group violin, in VVIQ-scale
#' units. Default 3 (i.e. violin extends from VVIQ=16 to VVIQ=13).
#' @param vviq_breaks Breaks (ticks) for the VVIQ x-axis
#' @param tas_breaks Breaks for the outcome variable (the main one being tas).
#' @param dot_size Size of the individual above-floor data points. Default
#' is 1.2.
#' @param dot_alpha Transparency of the individual above-floor data points.
#' Default is 0.4.
#' @param cross_size Size of the cross at the end of the regression line.
#' Default is 2.
#' @param stat_txt_size Size for the floor effect text on the left.
#' @param xbar_label Right-side "mean" label. Default is "Sample mean" (with a
#' linebreak). Another former option was expression(bar(x)) to display a 
#' mathematical X-bar symbol. 
#' @param xbar_face Face of the right label. Default is "italic".
#' @param xbar_vjust Vertical adjustment for the right-side "mean" label
#' (formerly X-bar). Default is 0.5.
#' @param colorbar_width Width of the colorbar in the legend in pt. Default is
#' 14
#' @param mean_line_color Colour of the sample-mean reference line (dashed
#' horizontal line, labelled via the right-side X-bar axis). Default is
#' "grey40".
#' @param mean_line_width Line width of the sample-mean reference line.
#' Default is 0.2.
#' @param floor_fill_color Fill colour for the floor-group violin, jittered
#' points, and mean/CI point-range. Default is "#C44E52" (a muted red).
#' @param floor_line_color Line/outline colour for the floor-group violin
#' border and mean/CI point-range. Default is "#8B3A3E" (a darker red).
#' @param floor_violin_alpha Fill transparency of the floor-group violin.
#' Default is 0.6.
#' @param floor_violin_linewidth Border line width of the floor-group violin.
#' Default is 0.2.
#' @param floor_violin_nudge Horizontal nudge applied to the floor-group
#' violin, in VVIQ-scale units (negative moves it further left of VVIQ=16).
#' Default is -2.
#' @param floor_jitter_alpha Transparency of the floor-group's individual
#' jittered points. Default is 0.2.
#' @param floor_jitter_size Size of the floor-group's individual jittered
#' points. Default is 1.2.
#' @param floor_pointrange_linewidth Line width of the floor-group's mean/CI
#' point-range. Default is 0.5.
#' @param floor_pointrange_size Point size of the floor-group's mean/CI
#' point-range. Default is 0.4.
#' @param floor_guide_color Colour of the dotted vertical guide line marking
#' VVIQ=16, spanning the violin's range. Default is "grey82".
#' @param floor_guide_linewidth Line width of the dotted vertical guide line
#' at VVIQ=16. Default is 0.2.
#' @param floor_guide_y_pad Additional downward padding (in outcome-scale
#' units) below the violin's own range for the vertical guide line's lower
#' end. Default is 5.
#' @param floor_label_x Horizontal position (VVIQ-scale) of the "Floor VVIQ"
#' text label. Default is 15.2.
#' @param floor_label_y Vertical position (outcome-scale) of the "Floor VVIQ"
#' text label. Default is 20.
#' @param floor_label_color Colour of the "Floor VVIQ" text label. Default is
#' "grey65".
#' @param floor_label_size Text size of the "Floor VVIQ" text label. Default
#' is 1.75.
#' @param arrow_x Horizontal position (VVIQ-scale) of the two-way arrow and
#' its connecting tick segments, marking the gap between the cross and the
#' floor-group mean. Default is 10.5.
#' @param arrow_linewidth Line width of the two-way arrow. Default is 0.3.
#' @param arrow_length Length of the two-way arrow. Default is 0.05.
#' @param tick_linewidth Line width of the two short horizontal dotted tick
#' segments connecting the arrow's ends to the cross and floor-group mean.
#' Default is 0.2.
#' @param stat_label_x Horizontal position (VVIQ-scale) of the floor-effect
#' statistics text label, to the left of the arrow. Default is 8.2.
#' @param stat_label_lineheight Lineheight of the stat label (to tune the
#' mandatory linebreak). Default is 0.9 (ggplot2's default).
#' @param fitted_line_color Colour of the fitted above-floor regression
#' line (both solid and dashed/extrapolated segments). Default is "black".
#' @param fitted_line_width Line width of the solid, real-data-range
#' portion of the fitted regression line. Default is 0.5.
#' @param extrap_line_width Line width of the dashed, extrapolated portion
#' of the fitted regression line. Default is 0.4.
#' @param cross_stroke Stroke width of the cross marker. Default is 0.8.
#' @param base_theme Base ggplot2 theme to use with [theme_pdf()]
#' (default is `ggplot2::theme_minimal`).
#' @param axis_relative_size A numeric value for the relative size of the axis
#' text compared to the base size. The default is 0.85, which is slightly
#' smaller than the base size.
#' @param axis_relative_x Relative size of the x-axis text. Default is 1.
#' @param axis_relative_y Relative size of the y-axis text. Default is 1.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' further customization of the plot theme.
#'
#' @returns A ggplot2 object.
#' @export
plot_floor_group <- function(
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
) {
  # NOTE: the annotation x-positions below ("Floor VVIQ" label, arrow,
  # slope-label) were hand-tuned against a composed figure where the top
  # panel (plot_vviq_marginal_histogram()) forces
  # scale_x_continuous(limits = c(8, 81)). These positions aren't
  # structurally linked to that limit — if either function's default
  # x-expansion changes, re-check that annotations still land where
  # intended, especially if this plot is used standalone (without the
  # forced limits from the composed top panel).
  rlang::check_installed("modelbased")
  rlang::check_installed("marginaleffects")
  
  # ----------------------------------------------------------------------
  # Outcome variable, derived from the model object itself rather than
  # hardcoded — brms always stores the outcome as the first column of the
  # data it saves on the fitted object, so this reads a structural fact
  # about brmsfit objects rather than guessing. Makes this function work
  # identically on the total-TAS model and any of the three subscale
  # models (tas_identify, tas_describe, tas_external) without a new
  # argument, per the "use the model object only" constraint.
  # ----------------------------------------------------------------------
  outcome_var <- colnames(model$data)[1]
  outcome_vals <- data[[outcome_var]]
  
  # Plausible range for the density estimate below, derived from the
  # actual data rather than hardcoded to TAS-20 total's 20-100 range —
  # a subscale (e.g. tas_identify, 7 items on a 1-5 scale, plausible
  # range ~7-35) needs its own bounds, not TAS total's.
  outcome_min <- min(outcome_vals, na.rm = TRUE)
  outcome_max <- max(outcome_vals, na.rm = TRUE)
  
  # ----------------------------------------------------------------------
  # 1. Predictions across the FULL range from vviq=16 to the real
  # above-floor maximum, using the above-floor coefficients throughout
  # (i.e. "what would this relationship predict, including the
  # counterfactual stretch below the real above-floor minimum"). Split
  # into solid (real data range) vs. dashed (extrapolated) segments AFTER
  # generating one continuous prediction, rather than two separate grids —
  # simpler, and avoids the earlier bug where a tiny 2-point extrapolation
  # grid (17->16) was visually negligible and the "at vviq=16" point was
  # incorrectly indexed from the wrong end of that grid.
  # ----------------------------------------------------------------------
  above_floor_data <- data[data$complete_aphant == "above_floor", ]
  vviq_min_above <- min(above_floor_data$vviq)
  vviq_max_above <- max(above_floor_data$vviq)
  
  pred_grid <- data.frame(
    vviq = seq(16, vviq_max_above, length.out = 200),
    complete_aphant = factor("above_floor", levels = levels(data$complete_aphant))
  )
  all_preds <- 
    marginaleffects::predictions(
      model, 
      newdata = pred_grid, 
      re_formula = NA   # To allow the function to run on single- or multi-level
    )
  all_preds <- as.data.frame(all_preds)
  
  # Split by whether each point falls in the real above-floor data range
  # or the extrapolated stretch below it.
  line_preds   <- all_preds[all_preds$vviq >= vviq_min_above, ]
  extrap_preds <- all_preds[all_preds$vviq <= vviq_min_above, ]  # overlap at the boundary is deliberate, for a continuous-looking join
  
  # The "where the line wrongly predicts" point — genuinely AT vviq=16 now,
  # taken directly from all_preds rather than an indexing guess.
  extrap_at_16 <- all_preds[which.min(all_preds$vviq), ]
  
  # ----------------------------------------------------------------------
  # 2. Floor group's actual predicted mean + CI (real prediction, not
  # extrapolation — complete_aphant = "floor" here).
  # ----------------------------------------------------------------------
  floor_pred_grid <- data.frame(
    vviq = 16,
    complete_aphant = factor("floor", levels = levels(data$complete_aphant))
  )
  floor_pred <- 
    marginaleffects::predictions(
      model, 
      newdata = floor_pred_grid, 
      re_formula = NA # To allow the function to run on single- or multi-level
    )
  floor_pred <- as.data.frame(floor_pred)
  
  # ----------------------------------------------------------------------
  # 2b. Floor-effect statistics, computed LIVE from the model — same
  # coefficient (complete_aphant) and same describe_posterior() approach
  # already validated in parameter_evidence.R, so the annotation always
  # reflects the actual fitted model rather than a hardcoded string. ROPE
  # range follows the existing group-contrast convention (appropriate
  # here since complete_aphant is a discrete shift, not a slope — see
  # parameter_evidence.R for why slopes need a DIFFERENT convention).
  # ----------------------------------------------------------------------
  rlang::check_installed("bayestestR")
  floor_effect_stats <- bayestestR::describe_posterior(
    model,
    parameters = "complete_aphant",
    rope_range = bayestestR::rope_range(model)
  )
  floor_effect_label <- sprintf(
    "%.2f\n[%.2f, %.2f]",
    floor_effect_stats$Median, 
    floor_effect_stats$CI_low, floor_effect_stats$CI_high
  )
  
  # Above-floor vviq slope, for the plot caption (kept OUT of the panel
  # itself — this figure's single visual argument is the floor-group gap, and a
  # second in-panel annotation for the slope would compete with it. A caption
  # keeps the slope visible "at a glance" without diluting that focus.
  sd_outcome <- stats::sd(model$data[[outcome_var]])
  sd_vviq <- stats::sd(model$data$vviq)
  rope_range_slope <- 0.2 * (sd_outcome / sd_vviq)  # Cohen "small effect", rescaled
  
  slope_stats <- bayestestR::describe_posterior(
    model,
    parameters = "vviq", 
    rope_range = c(-rope_range_slope, rope_range_slope)
  )
  slope_caption <- sprintf(
    "Above-floor slope: %.2f [%.2f, %.2f], pd = %s%%",
    slope_stats$Median, slope_stats$CI_low, slope_stats$CI_high,
    format(slope_stats$pd * 100, digits = 3)
  )
  
  # ----------------------------------------------------------------------
  # 3. Floor-group half-violin (raw data, not model-based) — kernel density
  # of the floor group's OBSERVED tas values, placed to the left of vviq=16.
  # ----------------------------------------------------------------------
  floor_raw <- data[data$complete_aphant == "floor", ]
  dens <- stats::density(
    floor_raw[[outcome_var]], n = 200, 
    from = outcome_min, to = outcome_max)
  dens_scaled <- dens$y / max(dens$y) * violin_width
  violin_df <- data.frame(
    x = 16 - dens_scaled,
    xend = 16,
    y = dens$x
  )
  
  # ----------------------------------------------------------------------
  # 4. Assemble the plot
  # ----------------------------------------------------------------------
  p <-
    ggplot2::ggplot() +
    # Above-floor scatter, coloured by VVIQ (viridis) — deliberately NOT
    # using the package's usual scale_discrete_aphantasia() here, since
    # this figure's argument is that the above-floor group is ONE
    # continuum, not four categories (see chat discussion — a continuous
    # gradient better matches this figure's specific claim).
    ggplot2::geom_point(
      data = above_floor_data,
      ggplot2::aes(
        x = .data$vviq, 
        y = .data[[outcome_var]], 
        color = .data$vviq
      ),
      alpha = dot_alpha, size = dot_size
    ) +
    ggplot2::geom_segment(
      data = data.frame(
        x = 16 + floor_violin_nudge,
        xend = 81, 
        y = mean(outcome_vals)
      ),
      ggplot2::aes(
        x = .data$x, xend = .data$xend, 
        y = .data$y, yend = .data$y),
      color = mean_line_color,
      linewidth = mean_line_width,
      linetype = "dashed"
    ) +
    # ggplot2::geom_hline(
    #   # Sample-wide TAS mean, as a reference point for both the fitted
    #   # line and the floor group's mean — gives the reader a fixed anchor
    #   # to judge both against, rather than only comparing them to each
    #   # other. Labelled via a right-side secondary y-axis break (see
    #   # scale_y_continuous() below) rather than an in-panel annotation —
    #   # the left side of this figure is already crowded (violin, arrow,
    #   # stats label, "Floor VVIQ" text), so the right side is the better
    #   # home for this, and a genuine axis element won't get clipped or
    #   # collide with panel content the way an annotate() call could.
    #   yintercept = mean(outcome_vals),
    #   color = mean_line_color,
    #   linewidth = mean_line_width,
    #   linetype = "dashed"
    # ) +
    # Floor-group half-violin (raw density, not model-based)
    ggplot2::geom_polygon(
      data = rbind(
        data.frame(x = violin_df$x, y = violin_df$y),
        data.frame(x = rev(violin_df$xend), y = rev(violin_df$y))
      ),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = floor_fill_color, alpha = floor_violin_alpha,
      color = floor_line_color, linewidth = floor_violin_linewidth,
      position = ggplot2::position_nudge(x = floor_violin_nudge)
    ) +
    ggplot2::geom_segment(
      # Vertical guide at vviq=16, spanning from below the violin's visible
      # range up to the violin's own maximum — widened and switched from
      # dashed to DOTTED (was a shorter dashed segment down to just
      # floor_pred$estimate in the previous version) specifically so it
      # reads as a distinct line style from the sample-mean dashed hline
      # above and from the arrow-to-point dotted "tick" segments below —
      # three different line roles (mean reference, floor-vviq marker,
      # arrow-endpoint ticks) needed three visually distinct treatments to
      # avoid the figure reading as "lots of vaguely similar dashed lines".
      data = data.frame(
        x = 16,
        y = min(violin_df$y) - floor_guide_y_pad,
        yend = max(violin_df$y)
      ),
      ggplot2::aes(
        x = .data$x, xend = .data$x, 
        y = .data$y, yend = .data$yend),
      linetype = "dotted",
      linewidth = floor_guide_linewidth,
      color = floor_guide_color
    ) +
    ggplot2::annotate(
      geom = "text",
      x = floor_label_x,
      y = floor_label_y,
      label = "Floor VVIQ",
      color = floor_label_color,
      angle = 90,
      size = floor_label_size
    ) +
    # Two-way arrow between the cross (where the shared line predicts) and
    # the floor group's actual mean, placed OUTSIDE the violin (further
    # left than the violin's own extent and the "Floor VVIQ" label above,
    # so it doesn't collide with either). Stats computed live in step 2b.
    #
    # PROBLEM SOLVED BY THE TWO SHORT DOTTED SEGMENTS BELOW: with the arrow
    # alone, sitting well to the left of both the cross and the floor-group
    # dot, it wasn't visually obvious WHICH two points the arrow's ends
    # actually corresponded to — a reader could plausibly read it as
    # pointing at the violin, or at some other pair of features nearby.
    # The two short horizontal dotted "tick" segments connect each arrow
    # endpoint directly across to its corresponding real point (the cross
    # and the floor-group mean dot respectively), removing that ambiguity.
    # Dotted (not dashed) specifically to stay visually distinct from both
    # the sample-mean dashed hline and the floor-vviq dotted vertical guide.
    ggplot2::annotate(
      geom = "segment",
      x = arrow_x, xend = arrow_x,
      y = extrap_at_16$estimate, yend = floor_pred$estimate,
      arrow = ggplot2::arrow(
        ends = "both", 
        length = grid::unit(arrow_length, "inches")),
      linewidth = arrow_linewidth,
      color = "black"
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = arrow_x, xend = 16,
      y = floor_pred$estimate, yend = floor_pred$estimate,
      linewidth = tick_linewidth,
      linetype = "dotted",
      color = "black"
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = arrow_x, xend = 16,
      y = extrap_at_16$estimate, yend = extrap_at_16$estimate,
      linewidth = tick_linewidth,
      linetype = "dotted",
      color = "black"
    ) +
    ggplot2::annotate(
      geom = "text",
      x = stat_label_x,
      y = (extrap_at_16$estimate + floor_pred$estimate) / 2,
      label = floor_effect_label,
      angle = 90,
      size = stat_txt_size,
      fontface = "bold",
      hjust = 0.5,
      lineheight = stat_label_lineheight
    ) +
    # Floor-group raw points, jittered leftward from vviq=16, same colour
    # family as the violin
    ggplot2::geom_jitter(
      data = data.frame(
        x = 16,
        y = floor_raw[[outcome_var]]
      ),
      ggplot2::aes(x = .data$x, y = .data$y),
      color = floor_fill_color, 
      alpha = floor_jitter_alpha, 
      size  = floor_jitter_size
    ) +
    # Fitted line, above-floor range (solid)
    ggplot2::geom_line(
      data = line_preds,
      ggplot2::aes(x = .data$vviq, y = .data$estimate),
      color = fitted_line_color, linewidth = fitted_line_width
    ) +
    # Extrapolated continuation (dashed) down to vviq=16
    ggplot2::geom_line(
      data = extrap_preds,
      ggplot2::aes(x = .data$vviq, y = .data$estimate),
      color = fitted_line_color, linewidth = extrap_line_width, linetype = "dashed"
    ) +
    # Floor group's actual mean + CI
    ggplot2::geom_pointrange(
      data = floor_pred,
      ggplot2::aes(
        x = .data$vviq, y = .data$estimate, 
        ymin = .data$conf.low, ymax = .data$conf.high),
      color     = floor_line_color, 
      linewidth = floor_pointrange_linewidth, 
      size      = floor_pointrange_size
    ) +
    # Cross marker at "where the line wrongly predicts"
    ggplot2::geom_point(
      data = extrap_at_16,
      ggplot2::aes(x = .data$vviq, y = .data$estimate),
      shape = 4, 
      size = cross_size, 
      stroke = cross_stroke, 
      color = "black"
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, caption = slope_caption) +
    ggplot2::scale_x_continuous(
      limits = limits,
      breaks = vviq_breaks, 
      expand = ggplot2::expansion(c(0.03, 0))) +
    ggplot2::scale_y_continuous(
      breaks = tas_breaks, 
      expand = ggplot2::expansion(c(0, 0.01)),
      # Right-side secondary axis, showing ONLY the sample-mean break,
      # labelled with the x-bar symbol via plotmath. This is a genuine
      # axis element (drawn by ggplot2's own axis-rendering machinery),
      # so it sits cleanly outside the panel and stays correctly aligned
      # with the dashed hline even if the plot is resized/composed.
      #
      # VERSION CAVEAT: sec_axis()'s transform argument was renamed from
      # `trans` to `transform` in a recent ggplot2 version. `~.` (identity)
      # is used here since no actual transformation is needed — if this
      # errors with "unused argument", try `trans = ~.` instead of
      # `transform = ~.` (or vice versa) depending on your installed
      # ggplot2 version.
      sec.axis = ggplot2::sec_axis(
        transform = ~.,
        breaks = mean(outcome_vals),
        labels = xbar_label
        # labels = "Sample\nmean"
        # labels = expression(bar(x))
        # labels = "x\u0304" # Wrong symbol
        # labels = expression(symbol("\xd7"))
      )) +
    ggplot2::scale_color_viridis_c(name = "VVIQ\n(above floor)") +
    theme_pdf(
      base_theme = base_theme, 
      axis_relative_size = axis_relative_size,
      axis_relative_x = axis_relative_x,
      axis_relative_y = axis_relative_y,
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(color = floor_guide_color),
      axis.text.y.right = ggplot2::element_text(
        # family = "sans",
        face = xbar_face,
        hjust = 0,
        vjust = xbar_vjust
      ),
      legend.key.width     = grid::unit(colorbar_width, "pt"),
      ...
    )
  
  return(p)
}