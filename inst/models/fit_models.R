devtools::load_all()

refit <- "always"

lm_prior <-
  fit_brms_model(
    formula = tas ~ vviq_group_4,
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    sample_prior = "only",
    file_refit = refit,
    file = here::here("inst/models/lm_prior.rds")
  )

lm_tot <-
  fit_brms_model(
    formula = tas ~ vviq_group_4,
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = refit,
    file = here::here("inst/models/lm_tot.rds")
  )

gam_tot <-
  fit_brms_model(
    formula = tas ~ s(vviq),
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = refit,
    file = here::here("inst/models/gam_tot.rds")
  )

lm_dif <- 
  fit_brms_model(
    formula = tas_identify ~ vviq_group_4, 
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = refit,
    file = here::here("inst/models/lm_dif.rds")
  )
lm_ddf <-
  fit_brms_model(
    formula = tas_describe ~ vviq_group_4, 
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = refit,
    file = here::here("inst/models/lm_ddf.rds")
  )
lm_eot <-
  fit_brms_model(
    formula = tas_external ~ vviq_group_4,
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = refit,
    file = here::here("inst/models/lm_eot.rds")
  )

gam_dif <- fit_brms_model(
  formula = tas_identify ~ s(vviq), 
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = refit,
  file = here::here("inst/models/gam_dif.rds")
)
gam_ddf <- fit_brms_model(
  formula = tas_describe ~ s(vviq), 
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = refit,
  file = here::here("inst/models/gam_ddf.rds")
)
gam_eot <- fit_brms_model(
  formula = tas_external ~ s(vviq),
  data = all_data,
  prior = c(brms::prior(normal(0, 20), class = "b")),
  file_refit = refit,
  file = here::here("inst/models/gam_eot.rds")
)
