devtools::load_all()

lm_2g <- lm(tas ~ vviq_group_2, data = all_data)
lm_3g <- lm(tas ~ vviq_group_3, data = all_data)
lm_4g <- lm(tas ~ vviq_group_4, data = all_data)
lm_c <- lm(tas ~ vviq, data = all_data)

lm_4gb <-
  fit_brms_model(
    formula = tas ~ vviq_group_4,
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/lm_tot.rds")
  )
gam_b <-
  fit_brms_model(
    formula = tas ~ s(vviq),
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/gam_tot.rds")
  )
gam_c <- mgcv::gam(tas ~ s(vviq), data = all_data)

mars <- earth::earth(tas ~ vviq, data = all_data)

performance::compare_performance(
  lm_2g, lm_3g, lm_4g, lm_4gb, 
  lm_c, gam_c, gam_b,
  metrics = c("LOOIC"),
  rank = FALSE
)