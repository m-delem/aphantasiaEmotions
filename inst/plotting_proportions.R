# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, scales, tidyr)
source("R/ggplot_tools.R")

load(here::here("data/tas_data.rda"))

m <- 8

p_props <- 
  tas_data |> 
  group_by(vviq_group_4, tas_group, study) |> 
  count() |>
  group_by(vviq_group_4, study) |>
  mutate(
    n_group = sum(n),
    prop = n / n_group
  ) |> 
  bind_rows(
    tas_data |> 
      group_by(vviq_group_4, tas_group) |> 
      count() |>
      group_by(vviq_group_4)  |>
      mutate(
        study = "total",
        n_group = sum(n),
        prop = n / n_group
      )
  ) |>
  mutate(
    study = factor(
      study, 
      levels = c("burns", "monzel", "ruby", "kvamme", "total")
    )
  ) |> 
  ggplot(
    aes(
      x = vviq_group_4,
      y = prop,
      fill = tas_group,
      colour = tas_group
    )
  ) +
  geom_bar(
    stat = "identity", 
    position = "fill",
    alpha = 0.3
  ) +
  geom_text(
    # aes(label = n),
    aes(label = ifelse(
      prop >= 0.16,
      paste0(round(prop * 100, 1), "%"),
      ""
    )),
    color = "black",
    size = 1.75,
    position = ggplot2::position_fill(vjust = 0.5)
  ) +
  facet_wrap(
    vars(study),
    ncol = 5,
    labeller = as_labeller(c(
      "burns"  = "Ale & Burns (2024)",
      "monzel" = "Monzel et al. (2024)",
      "ruby"   = "Ruby (2025)",
      "kvamme" = "Kvamme et al. (2025)",
      "total"  = "All studies combined"
    )),
    axes = "all_x"
  ) +
  scale_x_discrete(
    # expand = c(0, 0),
    limits = c(
      "hyperphantasia",
      "typical",
      "hypophantasia",
      "aphantasia"
    ),
    labels = c(
      "aphantasia"     = "Aphantasia",
      "hypophantasia"  = "Hypophantasia",
      "typical"        = "Typical",
      "hyperphantasia" = "Hyperphantasia"
    )
  ) +
  scale_y_reverse(expand = c(0.025, 0)) +
  coord_flip() +
  scale_discrete_manual(
    aes = c("fill", "colour"),
    name = NULL,
    labels = c(
      "alexithymia" = "Alexithymia",
      "typical_tas" = "No Alexithymia"
    ),
    values = palette.colors()[c(2, 3)]
  ) +
  labs(
    # title = "Proportions of alexithymia (TAS > 60) within VVIQ groups",
    # subtitle = write_sample_info(tas_data, n_groups = 4),
    x = NULL,
    y = "Proportion within the VVIQ group"
  ) +
  theme_pdf(
    theme_minimal,
    axis_relative_size = 1,
    axis_relative_y = 0.85,
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(angle = 0, hjust = 1),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.margin = margin(m, m, m, m),
    plot.caption = ggplot2::element_text(
      margin = margin(t = m), 
      color = "grey40"
    ),
    legend.position = "bottom"
  )

save_ggplot(
  plot = p_props, 
  path = here::here("inst/figures/alexithymia_proportions.pdf"),
  ncol = 2,
  height = 50,
  return = TRUE
)
