if(!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages(
    "cmdstanr",
    repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
  )
}
pacman::p_load(brms, cmdstanr, dplyr, ggplot2, patchwork)
devtools::load_all()

m_gam <- fit_brms_model(
  formula = tas ~ s(vviq), 
  data = tas_data,
  file = here::here("inst/models/brm_gam.rds")
  )

slopes <- 
  modelbased::estimate_slopes(
    m_gam, 
    trend = "vviq", 
    by = "vviq", 
    length = 75
  )

viz <- modelbased::visualisation_recipe(slopes)

viz$l2 <- create_evidence_groups(viz$l2)
viz$l3 <- create_evidence_groups(viz$l3, color = TRUE)

plot(viz) +
  ggplot2::scale_discrete_manual(
    aesthetics = c("color", "fill"),
    name = "Evidence",
    values = palette.colors()[c(1, 4)]
  ) +
  gam_style()

plot_gam_means(
  m_gam,
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

m_tot <- 
  fit_brms_model(
    formula = tas ~ vviq_group_4, 
    data = tas_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/brm_tot.rds")
  )
m_dif <- 
  fit_brms_model(
    formula = tas_identify ~ vviq_group_4, 
    data = tas_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/brm_dif.rds")
  )
m_ddf <-
  fit_brms_model(
    formula = tas_describe ~ vviq_group_4, 
    data = tas_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/brm_ddf.rds")
  )
m_eot <-
  fit_brms_model(
    formula = tas_external ~ vviq_group_4,
    data = tas_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/brm_eot.rds")
  )

brm_contrasts(m_grp, "vviq_group_4", plot = TRUE) |> 
  save_ggplot(
    path = here::here(
      "inst/visualisation/paper/fig_vviq_tas_total_contrasts.pdf"),
    ncol = 2,
    height = 100,
    return = TRUE
  )

