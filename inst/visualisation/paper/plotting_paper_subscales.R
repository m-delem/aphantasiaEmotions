devtools::load_all()
pacman::p_load(ggplot2, patchwork, superb)

source(here::here("inst/modelling_tas.R"))

size_star <- 3

# Group analyses
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
  customise_superb_plot(title = "Difficulty identifying feelings") |> 
  fix_superb_aes() +
  scale_discrete_aphantasia() +
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = size_star,
    tibble::tibble(
      x_star = 1.5,
      y_star = 33,
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
      y_star = 32,
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
      y_star = 29.5,
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
      y_star = 28.5,
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
      y_line = .data$y_star - 0.7
    )
  ) 

# EOT
m_external <- lm(tas_external ~ vviq_group_4, data = tas_data)
report_contrast(m_external, ~vviq_group_4)
pg_eot <-
  superb_with_params(tas_external ~ vviq_group_4, data = tas_data) |>
  customise_superb_plot(title = "Difficulty identifying feelings") |> 
  fix_superb_aes() +
  scale_discrete_aphantasia()

p_group_subscales <- 
  pg_dif + pg_ddf + pg_eot + 
  plot_layout(axis_titles = "collect") &
  theme(
    plot.title = element_text(size = 7),
    # plot.margin = margin(3.5, 3.5, 10, 3.5)
  )

# GAM means ---------------------------------------------
dot_size <- 0.5

pm_dif <-
  plot_gam_means(
    m_gam_identify,
    title = "Difficulty identifying feelings"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_identify,
    size = dot_size
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_ddf <- 
  plot_gam_means(
    m_gam_describe,
    title = "Difficulty describing feelings"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_describe,
    size = dot_size
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_eot <- 
  plot_gam_means(
    m_gam_external,
    title = "Externally oriented thinking"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_external,
    size = dot_size
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_gam_subscales <-
  pm_dif + pm_ddf + pm_eot +
  plot_layout(axes = "collect", guides = "collect") &
  theme(
    plot.title = element_text(size = 7),
    legend.position = "bottom",
    legend.box.spacing = unit(6, "pt"),
    # plot.margin = margin(10, 3.5, 10, 3.5)
  )

# GAM slopes -------------------------------------------------
ps_dif <- 
  plot_gam_slopes(
    m_gam_identify,
    title = "Difficulty identifying feelings",
    y_title = "VVIQ slope"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_ddf <- 
  plot_gam_slopes(
    m_gam_describe,
    title = "Difficulty describing feelings",
    y_title = "VVIQ slope"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_eot <- 
  plot_gam_slopes(
    m_gam_external,
    title = "Externally oriented thinking",
    y_title = "VVIQ slope"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_gam_subscales <-
  ps_dif + ps_ddf + ps_eot +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(
    plot.title = element_text(size = 7),
    legend.position = "bottom",
    legend.box.spacing = unit(6, "pt"),
    # plot.margin = margin(10, 3.5, 3.5, 3.5)
  )


# Combine plots -----------------------------------------------------------
p_subscales <-
  ggpubr::ggarrange(
      p_group_subscales,
      pm_gam_subscales,
      ps_gam_subscales,
      ncol = 1,
      labels = "AUTO",
      font.label = list(size = 11, face = "bold")
  )
  
save_ggplot(
  plot = p_subscales, 
  path = here::here("inst/visualisation/paper/fig_vviq_tas_subscales.pdf"),
  ncol = 2,
  height = 180,
  return = TRUE
)
