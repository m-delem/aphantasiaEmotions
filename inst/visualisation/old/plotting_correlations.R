# Plot correlations --------------------------------

p_combined <-
  plot_correlation(
    y = "tas",
    title = "Spearman correlation between VVIQ and TAS scores"
  ) +
  scale_discrete_aphantasia() +
  theme_pdf()

p_per_study <-
  plot_correlation(
    df = tas_data,
    grouping = grouping,
    n_groups = n_groups,
    title = "Spearman correlations between VVIQ and TAS scores",
    data_name = paste0(
      "The dotted line represents a non-linear LOESS modelling of the ",
      "VVIQ-TAS relationship."
    )
  ) +
  facet_wrap(
    vars(study),
    ncol = 3,
    labeller = as_labeller(c(
      "burns"  = "Burns et al. (2024)",
      "monzel" = "Monzel et al. (2024)",
      "ruby"   = "Ruby (2025)"
    ))
  )

save_ggplot(
  plot = p_combined, 
  path = here::here("inst/figures/vviq_tas_correlation_combined.pdf"),
  ncol = 2,
  height = 130,
  show = TRUE
)

save_ggplot(
  plot = p_per_study, 
  path = here::here("inst/figures/vviq_tas_correlation_per_study.pdf"),
  ncol = 2,
  height = 90,
  show = FALSE
)
