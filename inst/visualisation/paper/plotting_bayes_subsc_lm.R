devtools::load_all()
pacman::p_load(brms, patchwork)

# Models -------------------------------------
lm_dif <- 
  fit_brms_model(
    formula = tas_identify ~ vviq_group_4, 
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/lm_dif.rds")
  )
lm_ddf <-
  fit_brms_model(
    formula = tas_describe ~ vviq_group_4, 
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/lm_ddf.rds")
  )
lm_eot <-
  fit_brms_model(
    formula = tas_external ~ vviq_group_4,
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file = here::here("inst/models/lm_eot.rds")
  )

# Visualisation -------------------------------
p_dif <- 
  plot_group_violins(
    tas_identify ~ vviq_group_4, 
    y_lab = "TAS DIF Score",
    axis_relative_x = 0.8
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))
p_ddf <- 
  plot_group_violins(
    tas_describe ~ vviq_group_4, 
    y_lab = "TAS DDF Score",
    axis_relative_x = 0.8
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))
p_eot <- 
  plot_group_violins(
    tas_external ~ vviq_group_4, 
    y_lab = "TAS EOT Score",
    axis_relative_x = 0.8
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))

p_dif_contr <- 
  brm_contrasts(
    lm_dif, nudge_mult = 1.5, exp_mult = 0.4,
    xlab = "Effect size (DIF score difference)"
  )
p_ddf_contr <- 
  brm_contrasts(
    lm_ddf, nudge_mult = 1.5, exp_mult = 0.4,
    xlab = "Effect size (DDF score difference)"
  )
p_eot_contr <- 
  brm_contrasts(
    lm_eot, nudge_mult = 1.5, exp_mult = 0.4,
    xlab = "Effect size (EOT score difference)"
  )

p_sub_lm <-
  ((p_dif + p_dif_contr) + plot_layout(tag_level = "new")) / 
  ((p_ddf + p_ddf_contr) + plot_layout(tag_level = "new")) / 
  ((p_eot + p_eot_contr) + plot_layout(tag_level = "new")) + 
  plot_annotation(tag_levels = c("A", "1")) &
  ggplot2::theme(plot.tag = ggplot2::element_text(size = 10, face = "bold"))

save_ggplot(
  plot = p_sub_lm, 
  path = here::here("inst/visualisation/paper/fig_tas_subscales_lm.pdf"),
  ncol = 2,
  height = 180,
  return = TRUE
)
