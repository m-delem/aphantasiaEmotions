devtools::load_all()
pacman::p_load(brms, patchwork)

# Models -------------------------------------
m_tot <- 
  fit_brms_model(
    formula = tas ~ vviq_group_4, 
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/lm_tot.rds")
  )

m_gam <- 
  fit_brms_model(
    formula = tas ~ s(vviq), 
    data = all_data,
    file = here::here("inst/models/gam_tot.rds")
  )
slopes <-
  modelbased::estimate_slopes(m_gam, trend = "vviq", by = "vviq", length = 75)

# Visualisation -------------------------------
p_group <- 
  plot_group_violins(
    tas ~ vviq_group_4, 
    y_lab = "Total TAS Score",
    axis_relative_x = 0.8
  ) + 
  plot_alexithymia_cutoff(txt_x = 1.4, label = "Alexithymia") +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))

p_contr <- brm_contrasts(m_tot, plot = TRUE)

p_gam <-
  plot_gam_means(
    m_gam,
    y_title = "Total TAS score",
    legend_relative = 0.85
  ) +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas,
    size = 0.5
  ) + 
  plot_alexithymia_cutoff(txt_x = 26, label = "Alexithymia") +
  scale_discrete_aphantasia() +
  scale_x_vviq() 

check_slope_evidence(slopes)

p_slopes <-
  plot_brm_slopes(
    slopes,
    .f_groups = dplyr::case_when(
      vviq <= 23 ~ 1,
      vviq <= 33 ~ 2,
      vviq <= 36 ~ 3,
      vviq <= 47 ~ 4,
      vviq <= 71 ~ 5,
      vviq <= 80 ~ 6
    ),
    y_lab = "TAS variation per unit change in VVIQ",
  )

p_tot <-
  ((p_group | p_contr) + plot_layout(tag_level = "new")) / 
  ((p_gam | p_slopes) + plot_layout(tag_level = "new")) + 
  plot_annotation(tag_levels = c("A", "1")) &
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 10, face = "bold"))

save_ggplot(
  plot = p_tot, 
  path = here::here("inst/visualisation/paper/fig_tas_total.pdf"),
  ncol = 2,
  height = 140,
  return = TRUE
)
