# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, scales, tidyr)

source(here::here("R/plot_correlation.R"))
source("R/ggplot_utils.R")

load(here::here("data/tas_data.rda"))

# Plot correlations --------------------------------

grouping <- "vviq_group_4"
n_groups <- 4

p_combined <-
  plot_correlation(
    df = tas_data,
    grouping = grouping,
    n_groups = n_groups,
    data_name = paste0(
      "Combined data from Burns et al. (2024), Monzel et al. (2024) & Ruby ",
      "(2025).\n",
      "The dotted line represents a non-linear LOESS modelling of the ",
      "VVIQ-TAS relationship."
    ),
    title = "Spearman correlation between VVIQ and TAS scores"
  )

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
