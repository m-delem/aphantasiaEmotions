devtools::load_all()
pacman::p_load(ggplot2, patchwork)

source(here::here("inst/modelling_tas.R"))

# Plotting total TAS results without surrounding text for the paper ---------

# GAM plots --------------------------------------------------
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

# Group comparison plot --------------------------------------------
p_group_total <- 
  plot_group_violins(tas ~ vviq_group_4, y_lab = "Total TAS Score") + 
  plot_alexithymia_cutoff(txt_x = 1.5, label = "Alexithymia") +
  # Significance annotations for contrasts ---
  # Aphantasia vs. Hypophantasia
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 1.5,
      y_star = 106,
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
      y_star = 104.5,
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
      y_star = 99,
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
      y_star = 97.5,
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
      y_star = 91,
      stars  = "***",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = .data$y_star - 1.5
    )
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
