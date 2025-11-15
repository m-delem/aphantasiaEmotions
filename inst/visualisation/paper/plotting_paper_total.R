devtools::load_all()
pacman::p_load(ggplot2, patchwork, superb)

source(here::here("inst/modelling_tas.R"))

# Plotting total TAS results without surrounding text for the paper ---------

# GAM plots
pm_total <-
  plot_gam_means(
    m_gam_total,
    y_title = "Total TAS score"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas
  ) + 
  plot_alexithymia_cutoff(txt_x = 26) +
  scale_discrete_aphantasia() +
  gam_style()

ps_total <-
  plot_gam_slopes(
    m_gam_total,
    y_title = "Slope (TAS variation per unit change in VVIQ)"
  ) +
  gam_style()

p_gam_total <- 
  pm_total + ps_total &
  theme(
    plot.caption = element_text(size = 5.5),
    legend.key.spacing.x = unit(4, "pt"),
    legend.position = "bottom"
  )

# Group comparison plot
p_group_total <- 
  superb_with_params(tas ~ vviq_group_4, data = tas_data) |> 
  fix_superb_aes() + 
  plot_alexithymia_cutoff(txt_x = 1.5, label = "Alexithymia") +
  # Significance annotations for contrasts ---
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 1.5,
      y_star = 105,
      stars  = "***",
      x_line = 1,
      x_line_end = 2,
      y_line = .data$y_star - 1.5
    )
  ) +
  # Aphantasia vs. Hyperphantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.5,
      y_star = 103.5,
      stars  = "***",
      x_line = 1,
      x_line_end = 4,
      y_line = .data$y_star - 1.5
    )
  ) +
  # Hypophantasia vs. Typical
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2.5,
      y_star = 97,
      stars  = "***",
      x_line = 2,
      x_line_end = 3,
      y_line = .data$y_star - 1.5
    )
  ) +
  # Hypophantasia vs. Hyperphantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.5,
      y_star = 95.5,
      stars  = "***",
      x_line = 2,
      x_line_end = 4,
      y_line = .data$y_star - 1.5
    )
  ) +
  # Typical vs. Hyperphantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.5,
      y_star = 88,
      stars  = "***",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - 1.5
    )
  ) +
  scale_x_aphantasia() +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  scale_discrete_aphantasia() +
  labs(
    # caption = write_sample_info(tas_data, n_groups = 4),
    x = NULL, 
    y = "Total TAS Score"
  ) +
  theme_pdf(
    base_theme = theme_minimal,
    axis_relative_size = 1,
    axis_relative_x = 1,
    legend_relative = 1,
    # Custom theme arguments
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.caption = ggplot2::element_text(
      margin = margin(t = 8),
      # size = rel(1),
      color = "grey40"
    ),
    # plot.margin = margin(b = 15)
  )

# Combine plots -----------------------------------------------------------
p_total <-
  p_group_total / p_gam_total + 
  plot_layout(heights = c(1.3, 1)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 11, face = "bold"))

save_ggplot(
  plot = p_total, 
  path = here::here("inst/visualisation/paper/fig_vviq_tas_total.pdf"),
  ncol = 2,
  height = 180,
  return = TRUE
)
