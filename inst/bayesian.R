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
  data = all_data,
  file = here::here("inst/models/brm_gam.rds")
  )

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

p_contr <- brm_contrasts(m_tot, "vviq_group_4", plot = TRUE)

save_ggplot(
  p_contr,
  path = here::here(
    "inst/visualisation/paper/fig_vviq_tas_total_contrasts.pdf"),
  ncol = 2,
  height = 100,
  return = TRUE
)

