source(here::here("inst/plotting_verbose_linear.R"))
source(here::here("inst/plotting_verbose_gam.R"))

# TAS total ---------
p_total <- p_group_total / p_gam_total + plot_layout(heights = c(1.3, 1))

save_ggplot(
  plot = p_total, 
  path = here::here("inst/figures/vviq_tas_modelling_total.pdf"),
  ncol = 2,
  height = 210,
  show = TRUE
)

# TAS subscales ---------
p_subscales <- 
  ggarrange(
    p_group_subscales,
    pm_gam_subscales,
    ps_gam_subscales,
    ncol = 1
  )

save_ggplot(
  plot = p_subscales, 
  path = here::here("inst/figures/vviq_tas_modelling_subscales.pdf"),
  ncol = 2,
  height = 230,
  show = TRUE
)
