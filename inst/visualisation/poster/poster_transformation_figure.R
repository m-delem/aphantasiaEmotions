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
    middle.linewidth = 0.5 * s,
    violin_flip = 1,
    violin_nudge = c(-0.2, 0.2),
  ) +
  scale_colour_manual(values = c(grey_dark, grey_dark)) +
  scale_fill_manual(values = c(grey_mid, grey_mid)) +
  scale_x_discrete(
    labels = c(
      aphantasia = glue::glue("Aphantasics\n(VVIQ \u2264 32)\nN = {n_aph}"),
      typical    = glue::glue("Imagers\n(VVIQ > 32)\nN = {n_typ}")
    ),
    expand = expansion(mult = 0.7)
  ) +
  coord_cartesian(ylim = tas_limits) +
  labs(
    title   = "The common approach",
    caption = glue::glue(
      "Conclusion: aphantasics are\nmore alexithymic ",
      "(\u0394 = {sprintf('%.1f', delta_2g)} points)"
    )
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      colour = grey_dark, hjust = 0.5, face = "bold", size = rel(1.15)
    ),
    plot.caption = element_text(
      colour = grey_dark, hjust = 0.5, size = rel(0.85)
    ),
    panel.border = element_rect(
      colour = "grey85", fill = NA, linewidth = 0.2 * s
    ),
    axis.title.x = element_text(margin = margin(t = 5 * s))
  )

# plot_group_violins() hardcodes alphas tuned for a small figure; at A0 the
# points and violins wash out, so lift them here rather than fork the function.
p_common$layers[[1]]$aes_params$alpha <- 0.30   # jittered points
p_common$layers[[2]]$aes_params$alpha <- 0.55   # mean crossbar
p_common$layers[[3]]$aes_params$alpha <- 0.50   # half violin

# 4. Panel B: the turn ----

p_arrow <-
  ggplot() +
  annotate(
    "segment",
    x = 0.12, xend = 0.88, y = 0.42, yend = 0.42,
    linewidth = 0.45 * s, colour = grey_mid,
    arrow = grid::arrow(length = grid::unit(0.5 * s, "mm"), type = "closed")
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.56, label = "Unless\u2026",
    family = "Montserrat", fontface = "italic",
    size = 2.2 * s, colour = grey_dark
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void()

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
  scale_x_vviq(
    limits = c(6, 81), 
    breaks = NULL,
    expand = expansion(mult = c(0.02, 0))) +
  annotate(
    "text",
    x = 31, y = 112, hjust = 0, lineheight = 0.95,
    label = glue::glue("Complete aphantasia (VVIQ = 16)\nN = {n_flr}"),
    family = "Montserrat", fontface = "bold",
    size = 2.3 * s, colour = floor_dk
  ) +
  annotate(
    "curve",
    x = 29.5, xend = 17.8, y = 118, yend = 126,
    curvature = 0.32, linewidth = 0.35 * s, colour = floor_dk,
    arrow = grid::arrow(length = grid::unit(0.45 * s, "mm"), type = "closed")
  ) +
  labs(title = "What is actually there") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.2 * s) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      colour = violet, hjust = 0.5, face = "bold", size = rel(1.15)
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
    limits = c(6, 81),
    stat_label_x = 7,
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
  coord_cartesian(ylim = tas_limits)

# Predicted/observed key, using the panel's own two markers so nobody has to
# hunt for a legend. Replaces the colourbar, which said nothing the x-axis
# was not already saying.
key_x <- 20
key_y <- c(93.5, 89, 85.7)

p_floor <-
  p_floor +
  annotate(
    "rect",
    xmin = 17.4, xmax = 47, ymin = 84.4, ymax = 97,
    fill = "white", alpha = 0.95
  ) +
  annotate(
    "point", x = key_x, y = key_y[1],
    shape = 4, size = 2 * s, stroke = 0.8 * s, colour = "black"
  ) +
  annotate(
    "text", x = key_x + 2.2, y = key_y[1], hjust = 0,
    label = "Predicted: 59",
    family = "Montserrat", fontface = "bold",
    size = 2.6 * s, colour = "black"
  ) +
  annotate(
    "point", x = key_x, y = key_y[2],
    shape = 21, size = 1.5 * s, stroke = 0.5 * s,
    fill = floor_red, colour = floor_dk
  ) +
  annotate(
    "text", x = key_x + 2.2, y = key_y[2], hjust = 0,
    label = "Observed: 51",
    family = "Montserrat", fontface = "bold",
    size = 2.6 * s, colour = "black"
  ) +
  annotate(
    "text", x = key_x + 2.2, y = key_y[3], hjust = 0,
    label = "the level of a typical imager",
    family = "Montserrat", fontface = "italic",
    size = 2.0 * s, colour = grey_dark
  ) +
  labs(caption = paste0(
    "Floor-group shift: -8.41 [-11.15, -5.66].    ",
    "Above-floor slope: -0.27 [-0.35, -0.19], pd = 99.9%.\n",
    "Best of six models by cross-validation; the runner-up, ",
    "free to break anywhere,\nalso broke the relationship close to the floor."
  )) +
  theme(
    legend.position = "none",
    plot.caption = element_text(
      hjust = 1, face = "italic", colour = "grey35",
      size = rel(0.72), lineheight = 1.1
    ),
    axis.title.x = element_text(margin = margin(t = 5 * s))
  )

# 7. Assembly ----

# A and S occupy the same rows, which is what makes patchwork align their
# panels and therefore their alexithymia scales. Q is the QR void.
design <- c(
  # area( 5,  1, 16,  9),   # A  common approach v1 (aligned with floor-group)
  area( 4,  1, 17,  9),   # A  common approach v2 (vertically in the middle)
  area( 6, 10, 10, 12),   # R  arrow
  area( 1, 13,  5, 30),   # H  histogram
  area( 6, 13, 19, 30),   # S  floor-group model
  # area( 1,  1,  4,  9)    # Q  QR void v1
  area( 18, 1, 19, 9)    # Q  QR void v2
)

poster_figure <-
  p_common + p_arrow + p_hist + p_floor + plot_spacer() +
  plot_layout(design = design)

# 8. Export ----

# theme_pdf() registers Montserrat with showtext, not with systemfonts, so
# ragg cannot resolve it on its own. Keep showtext on and align its nominal
# resolution with the device: showtext converts point sizes to pixels at
# showtext_opts(dpi), and any mismatch with the device's res silently
# rescales every piece of type in the figure.

out_dir <- here::here("inst/visualisation/poster")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

preview <- FALSE
res     <- if (preview) 60 else 300
file    <- file.path(
  out_dir,
  if (preview) "preview.png" else "fig_poster_transformation.png"
)

showtext::showtext_opts(dpi = res)

ragg::agg_png(file, width = fig_width, height = fig_height,
              units = "mm", res = res, background = "white")
print(poster_figure)
invisible(dev.off())

showtext::showtext_opts(dpi = 96)

cat("wrote", file, "\n")
cat("QR void:", round(fig_width * 9 / 30), "x", round(fig_height * 4 / 16), "mm\n")
