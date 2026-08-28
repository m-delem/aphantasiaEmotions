# ---------------------------------------------------------------------------- #
# Poster figure: "The common approach" -> "What is actually there"
#
# The transformation block of graphical_abstract_v3, rebuilt at A0 scale for
# the Glasgow poster. Same exported plotting layer as the paper figures, so
# nothing here re-implements a plot that already exists.
#
# One thing to know before editing: No showtext. theme_pdf() switches it on; the 
# export switches it back off so ragg renders Montserrat natively through 
# systemfonts. That removes showtext's nominal-DPI trap rather than working 
# around it.
#
# Differences from the square abstract:
#   - the curved callout points at the histogram spike, not at the violin
#   - the scatter carries a predicted/observed key instead of a colourbar
#   - the left panel is desaturated and states the conclusion it produces
#   - the top-left cell is left empty for the QR code, placed in PowerPoint
#
# Output: inst/visualisation/poster/fig_poster_transformation.png
#         751 x 530 mm at 300 dpi (8870 x 6259 px, ~3.4 MB)
# ---------------------------------------------------------------------------- #

devtools::load_all()

library(ggplot2)
library(patchwork)

# 1. Geometry ----

# The composite sits at x = 45-796 mm, y = 470-1000 mm on the A0 sheet.
fig_width  <- 751
fig_height <- 530

# Everything is scaled from the published one-column figure (88 mm wide,
# base_size 7) so the printed proportions match the paper's exactly.
right_frac <- 18 / 30
s          <- (fig_width * right_frac) / 88
base_size  <- 7 * s

# Shared outcome scale. Both panels get these, which is what makes the
# comparison across the arrow honest.
tas_limits <- c(20, 95)
tas_breaks <- seq(20, 90, 10)

grey_mid  <- "#9C9A93"
grey_dark <- "#5F5E5A"
floor_red <- "#C44E52"
floor_dk  <- "#8B3A3E"
violet    <- "#3A2352"

# 2. Data and model ----

model_floor <- readRDS(here::here(
  "inst/models/floor_group_additive_multilevel_tot.rds"
))
model_data  <- model_floor$data

d2 <-
  aphantasiaEmotions::all_data |>
  dplyr::filter(!is.na(.data$tas), !is.na(.data$vviq))

n_aph <- sum(d2$vviq_group_2 == "aphantasia")
n_typ <- sum(d2$vviq_group_2 == "typical")
n_flr <- sum(model_data$complete_aphant == "floor")

delta_2g <-
  d2 |>
  dplyr::group_by(.data$vviq_group_2) |>
  dplyr::summarise(m = mean(.data$tas), .groups = "drop") |>
  dplyr::pull(.data$m) |>
  (\(x) x[1] - x[2])()

# 3. Panel A: the common approach ----

p_common <-
  plot_group_violins(
    tas ~ vviq_group_2,
    data      = d2,
    y_lab     = "Alexithymia score (TAS-20)",
    x_lab     = "Visual imagery group",
    breaks    = tas_breaks,
    dot_size  = 0.5 * s,
    base_size = base_size,
    box.linewidth    = 0.1 * s,
    middle.linewidth = 0.5 * s
  ) +
  ggplot2::scale_colour_manual(values = c(grey_dark, grey_dark)) +
  ggplot2::scale_fill_manual(values = c(grey_mid, grey_mid)) +
  ggplot2::scale_x_discrete(labels = c(
    aphantasia = glue::glue("Aphantasics\n(VVIQ \u2264 32)\nN = {n_aph}"),
    typical    = glue::glue("Imagers\n(VVIQ > 32)\nN = {n_typ}")
  )) +
  ggplot2::coord_cartesian(ylim = tas_limits) +
  ggplot2::labs(
    title   = "The common approach",
    caption = glue::glue(
      "Conclusion: aphantasics are\nmore alexithymic ",
      "(\u0394 = {sprintf('%.1f', delta_2g)} points)"
    )
  ) +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(
      colour = grey_dark, hjust = 0.5, face = "bold", size = ggplot2::rel(1.15)
    ),
    plot.caption = ggplot2::element_text(
      colour = grey_dark, hjust = 0.5, size = ggplot2::rel(0.85)
    ),
    panel.border = ggplot2::element_rect(
      colour = "grey85", fill = NA, linewidth = 0.2 * s
    )
  )

# plot_group_violins() hardcodes alphas tuned for a small figure; at A0 the
# points and violins wash out, so lift them here rather than fork the function.
p_common$layers[[1]]$aes_params$alpha <- 0.30   # jittered points
p_common$layers[[2]]$aes_params$alpha <- 0.55   # mean crossbar
p_common$layers[[3]]$aes_params$alpha <- 0.50   # half violin

# 4. Panel B: the turn ----

p_arrow <-
  ggplot2::ggplot() +
  ggplot2::annotate(
    "segment",
    x = 0.12, xend = 0.88, y = 0.42, yend = 0.42,
    linewidth = 0.45 * s, colour = grey_mid,
    arrow = grid::arrow(length = grid::unit(0.5 * s, "mm"), type = "closed")
  ) +
  ggplot2::annotate(
    "text",
    x = 0.5, y = 0.56, label = "Unless\u2026",
    family = "Montserrat", fontface = "italic",
    size = 2.2 * s, colour = grey_dark
  ) +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::theme_void()

# 5. Panel C: the distribution ----

# The curved callout lives here rather than on the scatter: the two panels
# are column-aligned, so the spike and the violin are already visibly the
# same people, and the gesture is better spent naming the spike.
p_hist <-
  plot_vviq_marginal_histogram(
    model_data,
    y_lab           = "Participant count",
    base_size       = base_size,
    floor_linewidth = 0.2 * s
  ) +
  ggplot2::annotate(
    "text",
    x = 31, y = 112, hjust = 0, lineheight = 0.95,
    label = glue::glue("Complete aphantasia (VVIQ = 16)\nN = {n_flr}"),
    family = "Montserrat", fontface = "bold",
    size = 2.3 * s, colour = floor_dk
  ) +
  ggplot2::annotate(
    "curve",
    x = 29.5, xend = 17.8, y = 118, yend = 126,
    curvature = 0.32, linewidth = 0.35 * s, colour = floor_dk,
    arrow = grid::arrow(length = grid::unit(0.45 * s, "mm"), type = "closed")
  ) +
  ggplot2::labs(title = "What is actually there") +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(
      colour = violet, hjust = 0.5, face = "bold", size = ggplot2::rel(1.15)
    )
  )

# 6. Panel D: the floor-group model ----

p_floor <-
  plot_floor_group(
    model_floor,
    model_data,
    y_lab       = "Alexithymia score (TAS-20)",
    x_lab       = "Visual imagery vividness (VVIQ)",
    vviq_breaks = seq(16, 80, 8),
    tas_breaks  = tas_breaks,
    xbar_label  = "Sample\nmean",
    base_size   = base_size,
    dot_size    = 1.2 * s,
    cross_size  = 2 * s,
    cross_stroke = 0.8 * s,
    stat_txt_size     = 1.75 * s,
    floor_label_size  = 1.75 * s,
    floor_jitter_size = 1.2 * s,
    floor_pointrange_linewidth = 0.5 * s,
    floor_pointrange_size      = 0.4 * s,
    floor_violin_linewidth     = 0.2 * s,
    floor_guide_linewidth      = 0.2 * s,
    fitted_line_width = 0.5 * s,
    extrap_line_width = 0.4 * s,
    arrow_linewidth   = 0.3 * s,
    arrow_length      = 0.05 * s,
    tick_linewidth    = 0.2 * s,
    mean_line_width   = 0.2 * s,
    floor_label_color = "transparent"
  ) +
  ggplot2::coord_cartesian(ylim = tas_limits)

# Predicted/observed key, using the panel's own two markers so nobody has to
# hunt for a legend. Replaces the colourbar, which said nothing the x-axis
# was not already saying.
key_x <- 20
key_y <- c(94, 88.5, 83.2)

p_floor <-
  p_floor +
  ggplot2::annotate(
    "rect",
    xmin = 17.4, xmax = 47, ymin = 80.4, ymax = 97,
    fill = "white", alpha = 0.95
  ) +
  ggplot2::annotate(
    "point", x = key_x, y = key_y[1],
    shape = 4, size = 2 * s, stroke = 0.8 * s, colour = "black"
  ) +
  ggplot2::annotate(
    "text", x = key_x + 2.2, y = key_y[1], hjust = 0,
    label = "Predicted: 59",
    family = "Montserrat", fontface = "bold",
    size = 2.6 * s, colour = "black"
  ) +
  ggplot2::annotate(
    "point", x = key_x, y = key_y[2],
    shape = 21, size = 1.5 * s, stroke = 0.5 * s,
    fill = floor_red, colour = floor_dk
  ) +
  ggplot2::annotate(
    "text", x = key_x + 2.2, y = key_y[2], hjust = 0,
    label = "Observed: 51",
    family = "Montserrat", fontface = "bold",
    size = 2.6 * s, colour = "black"
  ) +
  ggplot2::annotate(
    "text", x = key_x + 2.2, y = key_y[3], hjust = 0,
    label = "the level of an ordinary imager",
    family = "Montserrat", fontface = "italic",
    size = 2.0 * s, colour = grey_dark
  ) +
  ggplot2::labs(caption = paste0(
    "Above-floor slope: -0.27 [-0.35, -0.19], pd = 99.9%.   ",
    "Floor-group shift: -8.41 [-11.15, -5.66].\n",
    "Best of six models by cross-validation; the runner-up, ",
    "free to break anywhere, broke there too."
  )) +
  ggplot2::theme(
    legend.position = "none",
    plot.caption = ggplot2::element_text(
      hjust = 1, face = "italic", colour = "grey35",
      size = ggplot2::rel(0.72), lineheight = 1.2
    )
  )

# 7. Assembly ----

# A and S occupy the same rows, which is what makes patchwork align their
# panels and therefore their alexithymia scales. Q is the QR void.
design <- c(
  patchwork::area( 5,  1, 16,  9),   # A  common approach
  patchwork::area( 9, 10, 11, 12),   # R  arrow
  patchwork::area( 1, 13,  4, 30),   # H  histogram
  patchwork::area( 5, 13, 16, 30),   # S  floor-group model
  patchwork::area( 1,  1,  4,  9)    # Q  QR void
)

poster_figure <-
  p_common + p_arrow + p_hist + p_floor + patchwork::plot_spacer() +
  patchwork::plot_layout(design = design)

# 8. Export ----

# ragg renders Montserrat natively through systemfonts, so showtext is turned
# back off after theme_pdf() switched it on. That sidesteps showtext's
# nominal-DPI trap entirely: no showtext_opts() dance, no wrongly-scaled type.
showtext::showtext_auto(FALSE)

out_dir <- here::here("inst/visualisation/poster")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Set preview <- TRUE while tuning: same layout, same millimetres, small file.
preview <- FALSE
res     <- if (preview) 60 else 300
file    <- file.path(
  out_dir,
  if (preview) "preview.png" else "fig_poster_transformation.png"
)

ragg::agg_png(file, width = fig_width, height = fig_height,
              units = "mm", res = res, background = "white")
print(poster_figure)
invisible(dev.off())

cat("wrote", file, "\n")
cat("QR void:", round(fig_width * 9 / 30), "x", round(fig_height * 4 / 16), "mm\n")
