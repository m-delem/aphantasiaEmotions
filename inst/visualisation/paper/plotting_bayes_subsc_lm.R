devtools::load_all()
pacman::p_load(brms, patchwork)

# Models -------------------------------------
lm_dif <- 
  fit_brms_model(
    formula = tas_identify ~ vviq_group_4, 
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/lm_dif.rds")
  )
lm_ddf <-
  fit_brms_model(
    formula = tas_describe ~ vviq_group_4, 
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/lm_ddf.rds")
  )
lm_eot <-
  fit_brms_model(
    formula = tas_external ~ vviq_group_4,
    data = all_data,
    prior = c(prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/lm_eot.rds")
  )

# Contrasts -----------------------------------
contr_dif <- 
  marginaleffects::comparisons(
    lm_dif,
    variables = list("vviq_group_4" = "pairwise"),
    draw_ids = 1:4000
  )
contr_ddf <-
  marginaleffects::comparisons(
    lm_ddf,
    variables = list("vviq_group_4" = "pairwise"),
    draw_ids = 1:4000
  )
contr_eot <-
  marginaleffects::comparisons(
    lm_eot,
    variables = list("vviq_group_4" = "pairwise"),
    draw_ids = 1:4000
  )
  
# Visualisation -------------------------------
p_dif <- 
  plot_group_violins(
    tas_identify ~ vviq_group_4, 
    y_lab = "TAS DIF Score"
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))
p_ddf <- 
  plot_group_violins(
    tas_describe ~ vviq_group_4, 
    y_lab = "TAS DDF Score"
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))
p_eot <- 
  plot_group_violins(
    tas_external ~ vviq_group_4, 
    y_lab = "TAS EOT Score"
  ) +
  scale_discrete_aphantasia() +
  scale_x_aphantasia(add = c(0.4, 0.7))

p_dif_contr <- 
  plot_posterior_contrasts(
    contr_dif, lm_dif,
    xlab = "Effect size (DIF score difference)", 
    rope_txt = 1.25,
    axis_relative_x = 0.7
  )
p_ddf_contr <- 
  plot_posterior_contrasts(
    contr_ddf, lm_ddf,
    xlab = "Effect size (DDF score difference)", 
    rope_txt = 1.25,
    axis_relative_x = 0.7
  )
p_eot_contr <- 
  plot_posterior_contrasts(
    contr_eot, lm_eot,
    xlab = "Effect size (EOT score difference)", 
    rope_txt = 1.25,
    axis_relative_x = 0.7
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
