devtools::load_all()
pacman::p_load(brms, patchwork)

# Models -------------------------------------
gam_dif <- fit_brms_model(
  formula = tas_identify ~ s(vviq), 
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = "never",
  file = here::here("inst/models/gam_dif.rds")
  )
gam_ddf <- fit_brms_model(
  formula = tas_describe ~ s(vviq), 
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = "never",
  file = here::here("inst/models/gam_ddf.rds")
  )
gam_eot <- fit_brms_model(
  formula = tas_external ~ s(vviq),
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = "never",
  file = here::here("inst/models/gam_eot.rds")
  )

slopes_dif <-
  modelbased::estimate_slopes(gam_dif, trend = "vviq", by = "vviq", length = 75)
slopes_ddf <-
  modelbased::estimate_slopes(gam_ddf, trend = "vviq", by = "vviq", length = 75)
slopes_eot <-
  modelbased::estimate_slopes(gam_eot, trend = "vviq", by = "vviq", length = 75)

# Visualisation -------------------------------
p_gam_dif <-
  plot_gam_means(
    gam_dif,
    y_title = "TAS DIF score",
    legend_relative = 0.85
  ) +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas_identify,
    size = 0.5
  ) + 
  scale_discrete_aphantasia() +
  scale_x_vviq() 
p_gam_ddf <-
  plot_gam_means(
    gam_ddf,
    y_title = "TAS DDF score",
    legend.position = "none"
  ) +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas_describe,
    size = 0.5
  ) + 
  scale_discrete_aphantasia() +
  scale_x_vviq() 
p_gam_eot <-
  plot_gam_means(
    gam_eot,
    y_title = "TAS EOT score",
    legend.position = "none"
  ) +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas_external,
    size = 0.5
  ) + 
  scale_discrete_aphantasia() +
  scale_x_vviq() 
  
check_slope_evidence(slopes_dif)
p_slopes_dif <-
  plot_gam_slopes(
    slopes_dif,
    .f_groups = dplyr::case_when(
      vviq <= 22 ~ 1,
      vviq <= 31 ~ 2,
      vviq <= 38 ~ 3,
      vviq <= 57 ~ 4,
      vviq <= 74 ~ 5,
      vviq <= 80 ~ 6
    ),
    y_lab = "DIF variation per unit change in VVIQ",
    axis.title.y = ggplot2::element_text(size = 6)
  )


check_slope_evidence(slopes_ddf)
p_slopes_ddf <-
  plot_gam_slopes(
    slopes_ddf,
    .f_groups = dplyr::case_when(
      vviq <= 22 ~ 1,
      vviq <= 46 ~ 2,
      vviq <= 74 ~ 3,
      vviq <= 80 ~ 4
    ),
    y_lab = "DDF variation per unit change in VVIQ",
    legend.position = "none",
    axis.title.y = ggplot2::element_text(size = 6)
  )

check_slope_evidence(slopes_eot)
p_slopes_eot <-
  plot_gam_slopes(
    slopes_eot,
    .f_groups = dplyr::case_when(
      vviq <= 23 ~ 1,
      vviq <= 42 ~ 2,
      vviq <= 69 ~ 3,
      vviq <= 80 ~ 4
    ),
    y_lab = "EOT variation per unit change in VVIQ",
    legend.position = "none",
    axis.title.y = ggplot2::element_text(size = 6)
  )

p_sub_gam <-
  ((p_gam_dif + p_slopes_dif) + plot_layout(tag_level = "new")) / 
  ((p_gam_ddf + p_slopes_ddf) + plot_layout(tag_level = "new")) / 
  ((p_gam_eot + p_slopes_eot) + plot_layout(tag_level = "new")) + 
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(tag_levels = c("A", "1")) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 10, face = "bold")
  )

save_ggplot(
  plot = p_sub_gam, 
  path = here::here("inst/visualisation/paper/fig_tas_subscales_gam.pdf"),
  ncol = 2,
  height = 180,
  return = TRUE
)
