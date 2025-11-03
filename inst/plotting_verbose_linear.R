# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, scales)
source(here::here("R/report_contrast.R"))
source(here::here("R/ggplot_utils.R"))
source(here::here("R/plot_superb.R"))

load(here::here("data/tas_data.rda"))

# Total TAS score linear (group) model ---------------------
m_total <- lm(tas ~ vviq_group_4, data = tas_data)
report_contrast(m_total, ~vviq_group_4)

p_group_total <- 
  superb_with_params(tas ~ vviq_group_4, data = tas_data) + 
  plot_alexithymia_cutoff(txt_x = 1.5, label = "Alexithymia") +
  # Significance annotations for contrasts ---
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 1.5,
      y_star = 105,
      stars  = "*",
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
      stars  = "*",
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
      stars  = "*",
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
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - 1.5
    )
  ) +
  scale_x_discrete(
    labels = c(
      "aphantasia"     = "Aphantasia",
      "hypophantasia"  = "Hypophantasia",
      "typical"        = "Typical",
      "hyperphantasia" = "Hyperphantasia"
    ),
    expand = expansion(
      mult = 0,
      add = c(0, 0.6))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(10),
  ) +
  scale_discrete_manual(
    aesthetics = c("color", "fill"),
    values = palette.colors()
  ) +
  labs(
    x = NULL,
    y = "TAS Score",
    title = "TAS score differences between VVIQ groups (linear model contrasts)",
    subtitle = write_sample_info(tas_data, n_groups = 4),
    caption = paste0(
      "Combined data from Burns et al. (2024), Monzel et al. (2024) & ",
      "Ruby (2025). The red line indicates the alexithymia cut-off ",
      "(TAS > 60).\n",
      "The significance labels are based on a contrast analysis of a linear ",
      "model predicting TAS scores with VVIQ groups."
    )
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
    plot.margin = margin(b = 25)
  )

q <- ggplot_build(p_group_total)
q$plot$mapping <- 
  aes(
    x = vviq_group_4, 
    y = center, 
    colour = vviq_group_4, 
    fill = vviq_group_4,
    shape = NULL
  )

p_group_total <- q$plot
rm(q)

# Subscale linear (group) models -----------------------------------------------

size_star <- 3

# DIF
m_identify <- lm(tas_identify ~ vviq_group_4, data = tas_data)
report_contrast(m_identify, ~vviq_group_4)
pg_dif <-
  superb_with_params(tas_identify ~ vviq_group_4, data = tas_data) |> 
  customise_plot(title = "Difficulty identifying feelings") |> 
  fix_aes() +
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 1.5,
      y_star = 44.5,
      stars  = "°",
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
      stars  = "*",
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
      stars  = "**",
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
      stars  = "*",
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
  customise_plot(title = "Difficulty describing feelings") |> 
  fix_aes() +
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 1.5,
      y_star = 33,
      stars  = "°",
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
      y_star = 32,
      stars  = "**",
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
      y_star = 29,
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
      y_star = 28,
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
      y_star = 25,
      stars  = "*",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - 1
    )
  ) 

# EOT
m_external <- lm(tas_external ~ vviq_group_4, data = tas_data)
report_contrast(m_external, ~vviq_group_4)
pg_eot <-
  superb_with_params(tas_external ~ vviq_group_4, data = tas_data) |>
  customise_plot(title = "Externally oriented thinking") |> 
  fix_aes() +
  # Aphantasia vs. Typical
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 2,
      y_star = 39.5,
      stars  = "*",
      x_line = 1,
      x_line_end = 3,
      y_line = .data$y_star - 1.5
    )
  )

p_group_subscales <-
  pg_dif + pg_ddf + pg_eot +
  plot_annotation(
    title = paste0(
      "TAS subscale score differences between VVIQ groups ",
      "(linear model contrasts)"
    ),
    subtitle = write_sample_info(tas_data, n_groups = 4),
    caption = paste0(
      "Combined data from Burns et al. (2024), Monzel et al. (2024) & ",
      "Ruby (2025).\n",
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
