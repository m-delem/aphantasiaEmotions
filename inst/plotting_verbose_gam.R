# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(ggplot2, patchwork)

source(here::here("inst/modelling_tas.R"))
source(here::here("R/plot_gam.R"))

load(here::here("data/tas_data.rda"))

# Plotting results of GAM models with titles, captions, etc. --------------

# Total score data
pm_total <-
  plot_gam_means(
    m_gam_total,
    title = "Non-linear relationship between TAS and VVIQ scores",
    subtitle = write_sample_info(tas_data, n_groups = 4),
    y_title = "TAS score",
    caption = paste0(
      "The black line represents a generalized additive model (GAM) fitted ",
      "to the data.\n",
      "The shaded area represents the 95% confidence interval of the GAM."
    )
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas
  ) + 
  plot_alexithymia_cutoff() +
  scale_discrete_aphantasia() +
  gam_style(plot_margin = margin(3.5, 10, 3.5, 3.5))

ps_total <-
  plot_gam_slopes(
    m_gam_total,
    title = "Non-linear variation of TAS scores by VVIQ",
    subtitle = "Estimation based on the first derivative of the GAM",
    caption = paste0(
      "A slope above 0 indicates that as VVIQ increases, TAS scores also ",
      "increase.\n",
      "A slope below 0 indicates that as VVIQ increases, TAS scores decrease."
    )
  ) +
  gam_style(plot_margin = margin(3.5, 8, 3.5, 10), h_caption = 1)

p_gam_total <- 
  pm_total + ps_total &
  theme(plot.caption = element_text(size = 5.5))

# Subscales
# Means
pm_dif <-
  plot_gam_means(
    m_gam_identify,
    title = "Difficulty identifying feelings"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_identify
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_ddf <- 
  plot_gam_means(
    m_gam_describe,
    title = "Difficulty describing feelings"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_describe
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_eot <- 
  plot_gam_means(
    m_gam_external,
    title = "Externally oriented thinking"
  ) +
  plot_coloured_subjects(
    df = tas_data,
    x = tas_data$vviq, 
    y = tas_data$tas_external
  ) +
  scale_discrete_aphantasia() +
  gam_style(axis_relative_size = 0.7)

pm_gam_subscales <-
  pm_dif + pm_ddf + pm_eot +
  plot_layout(axes = "collect", guides = "collect") +
  plot_annotation(
    title = "Non-linear relationships between TAS subscale scores and VVIQ",
    caption = paste0(
      "The black line represents generalized additive models (GAM) fitted ",
      "to the subscale data.\n",
      "The shaded area represents the 95% confidence interval of the GAMs."
    ),
    theme = theme_pdf(
      plot.title = element_text(
        size = 9, 
        hjust = 0.5, 
        margin = margin(t = 10, b = 5)
      ),
      plot.subtitle = element_text(
        size = 8, hjust = 0.5, face = "italic",
        margin = margin(b = 10)
      ),
      plot.caption = element_text(size = 7, color = "grey40", face = "italic")
    )
  ) &
  theme(legend.position = "bottom")

# Slopes
ps_dif <- 
  plot_gam_slopes(
    m_gam_identify,
    title = "Difficulty identifying feelings"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_ddf <- 
  plot_gam_slopes(
    m_gam_describe,
    title = "Difficulty describing feelings"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_eot <- 
  plot_gam_slopes(
    m_gam_external,
    title = "Externally oriented thinking"
  ) +
  gam_style(axis_relative_size = 0.7)

ps_gam_subscales <-
  ps_dif + ps_ddf + ps_eot +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(
    title = "Non-linear variations of TAS subscale scores by VVIQ",
    caption = paste0(
      "A slope above 0 indicates that as VVIQ increases, TAS subscale scores ",
      "also increase.\n",
      "A slope below 0 indicates that as VVIQ increases, TAS subscale scores ",
      "decrease."
    ),
    theme = theme_pdf(
      plot.title = element_text(
        size = 9, 
        hjust = 0.5, 
        margin = margin(t = 10, b = 5)
      ),
      plot.caption = element_text(size = 7, color = "grey40", face = "italic")
    )
  ) &
  theme(legend.position = "bottom")
