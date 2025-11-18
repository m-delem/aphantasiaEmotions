devtools::load_all()
pacman::p_load(ggplot2, patchwork, superb)

# Total TAS score linear (group) model ---------------------
m_total <- lm(tas ~ vviq_group_4, data = tas_data)
report_contrast(m_total, ~vviq_group_4)

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
    x = NULL,
    y = "TAS Score",
    title = "TAS score differences between VVIQ groups (linear model contrasts)",
    subtitle = write_sample_info(tas_data, n_groups = 4),
    caption = paste0(
      "Combined data from Ale & Burns (2024), Monzel et al. (2024), Kvamme et ",
      "al. (2025) and Ruby (2025).\n",
      "The red line indicates the alexithymia cut-off (TAS > 60).",
      "The significance labels are based on a \n",
      "contrast analysis of a linear ",
      "model predicting TAS scores with VVIQ groups."
    )
  ) +
  theme_pdf(
    base_theme = ggplot2::theme_minimal,
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
    plot.margin = margin(b = 25)
  )

# Subscale linear (group) models -----------------------------------------------

size_star <- 3

# DIF
m_identify <- lm(tas_identify ~ vviq_group_4, data = tas_data)
report_contrast(m_identify, ~vviq_group_4)
pg_dif <-
  superb_with_params(tas_identify ~ vviq_group_4, data = tas_data) |> 
  customise_superb_plot(title = "Difficulty identifying feelings") |> 
  fix_superb_aes() +
  scale_discrete_aphantasia() +
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 1.5,
      y_star = 44.5,
      stars  = "**",
      x_line = 1,
      x_line_end = 2,
      y_line = .data$y_star - 1
    )
  ) +
  # Aphantasia vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 43.5,
      stars  = "***",
      x_line = 1,
      x_line_end = 4,
      y_line = .data$y_star - 1
    )
  ) +
  # Hypophantasia vs. Typical
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 2.5,
      y_star = 40.5,
      stars  = "***",
      x_line = 2,
      x_line_end = 3,
      y_line = .data$y_star - 1
    )
  ) +
  # Hypophantasia vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 39.5,
      stars  = "***",
      x_line = 2,
      x_line_end = 4,
      y_line = .data$y_star - 1
    )
  ) +
  # Typical vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 36,
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - 1
    )
  ) 

# DDF
m_describe <- lm(tas_describe ~ vviq_group_4, data = tas_data)
report_contrast(m_describe, ~vviq_group_4)
pg_ddf <-
  superb_with_params(tas_describe ~ vviq_group_4, data = tas_data) |>
  customise_superb_plot(title = "Difficulty describing feelings") |> 
  fix_superb_aes() +
  scale_discrete_aphantasia() +
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 1.5,
      y_star = 33.5,
      stars  = "***",
      x_line = 1,
      x_line_end = 2,
      y_line = .data$y_star - 1
    )
  ) +
  # Aphantasia vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 32.5,
      stars  = "***",
      x_line = 1,
      x_line_end = 4,
      y_line = .data$y_star - 1
    )
  ) +
  # Hypophantasia vs. Typical
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 2.5,
      y_star = 30,
      stars  = "***",
      x_line = 2,
      x_line_end = 3,
      y_line = .data$y_star - 1
    )
  ) +
  # Hypophantasia vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 29,
      stars  = "***",
      x_line = 2,
      x_line_end = 4,
      y_line = .data$y_star - 1
    )
  ) +
  # Typical vs. Hyperphantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 3.5,
      y_star = 26,
      stars  = "***",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - .8
    )
  ) 

# EOT
m_external <- lm(tas_external ~ vviq_group_4, data = tas_data)
report_contrast(m_external, ~vviq_group_4)
pg_eot <-
  superb_with_params(tas_external ~ vviq_group_4, data = tas_data) |>
  customise_superb_plot(title = "Externally oriented thinking") |> 
  fix_superb_aes() + 
  scale_discrete_aphantasia()

p_group_subscales <-
  pg_dif + pg_ddf + pg_eot +
  plot_annotation(
    title = paste0(
      "TAS subscale score differences between VVIQ groups ",
      "(linear model contrasts)"
    ),
    subtitle = write_sample_info(tas_data, n_groups = 4),
    caption = paste0(
      "Combined data from Ale & Burns (2024), Monzel et al. (2024), Kvamme et ",
      "al. (2025) and Ruby (2025).\n",
      "The significance labels are based on contrast analyses of linear ",
      "models predicting TAS subscale scores with VVIQ groups."
    ),
    theme = theme_pdf(
      # plot.title = element_text(size = 9, hjust = 0.5),
      # plot.subtitle = element_text(size = 8, hjust = 0.5),
      plot.caption = element_text(size = 6, color = "grey40", face = "italic")
    )
  ) +
  plot_layout(axis_titles = "collect")
