devtools::load_all()
pacman::p_load(dplyr, ggplot2, patchwork)

m <- 8

p_counts <-
  all_data |>
  bind_rows(all_data |> mutate(study = "total")) |>
  mutate(
    study = factor(
      study, 
      levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
    )
  ) |> 
  ggplot(
    aes(
      x = 1,
      fill = vviq_group_4,
      colour = vviq_group_4
    )
  ) +
  geom_bar(
    position = "fill",
    alpha = 0.3,
    linewidth = 0.1
  ) +
  geom_text(
    data = 
      all_data |>
      bind_rows(all_data |> mutate(study = "total")) |>
      mutate(
        study = factor(
          study, 
          levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
        )
      ) |> 
      group_by(vviq_group_4, study) |>
      count() |>
      group_by(study) |>
      mutate(
        n_group = sum(n),
        prop = n / n_group
      ),
    aes(
      y = prop,
      label = ifelse(
        prop >= 0,
        # paste0(round(prop * 100, 1), "%"),
        n,
        ""
      )
    ),
    color = "black",
    size = 1.75,
    position = ggplot2::position_fill(vjust = 0.5)
  ) +
  facet_wrap(
    vars(study),
    ncol = 6,
    labeller = as_labeller(c(
      "burns"  = "Ale & Burns (2024)\nN = 192",
      "monzel" = "Monzel et al. (2024)\nN = 105",
      "mas"    = "Mas et al. (2025)\nN = 123",
      "ruby"   = "Ruby (2025)\nN = 205",
      "kvamme" = "Kvamme et al. (2025)\nN = 833",
      "total"  = "All studies combined\nN = 1,458"
    )),
    axes = "all_x"
  ) +
  labs(x = NULL, y = NULL) +
  scale_discrete_aphantasia() +
  # scale_y_reverse(expand = c(0.025, 0)) +
  # coord_flip() +
  theme_pdf(
    theme_minimal,
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.margin = margin(m, m, m, m),
    plot.caption = ggplot2::element_text(
      margin = margin(t = m), 
      color = "grey40"
    ),
    legend.position = "left",
    legend.text = element_text(hjust = 1),
    legend.text.position = "left",
    # panel.spacing.x = grid::unit(8, "mm")
    strip.text = ggplot2::element_text(size = 6)
  )

p_props <-
  all_data |> 
  group_by(vviq_group_4, tas_group, study) |> 
  count() |>
  group_by(vviq_group_4, study) |>
  mutate(
    n_group = sum(n),
    prop = n / n_group
  ) |> 
  bind_rows(
    all_data |> 
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
      levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
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
    alpha = 0.3,
    linewidth = 0.1
  ) +
  geom_text(
    # aes(label = n),
    aes(label = ifelse(
      prop >= 0.16,
      paste0(round(prop * 100, 1), "%"),
      ""
    )),
    color = "black",
    size = 1.5,
    position = ggplot2::position_fill(vjust = 0.5)
  ) +
  facet_wrap(
    vars(study),
    ncol = 6,
    labeller = as_labeller(c(
      "burns"  = "Ale & Burns (2024)",
      "monzel" = "Monzel et al. (2024)",
      "mas"    = "Mas et al. (2025)",
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
      "alexithymia" = "Alexithymia (TAS > 60)",
      "typical_tas" = "No Alexithymia"
    ),
    values = palette.colors()[c(7, 6)]
  ) +
  labs(
    # title = "Proportions of alexithymia (TAS > 60) within VVIQ groups",
    # subtitle = write_sample_info(all_data, n_groups = 4),
    x = NULL,
    y = "Proportion within the VVIQ group"
  ) +
  theme_pdf(
    theme_minimal,
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(
      size = 7, 
      color = "black",
      angle = 0, hjust = 1
    ),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.margin = margin(m, m, m, m),
    plot.caption = ggplot2::element_text(
      margin = margin(t = m), 
      color = "grey40"
    ),
    legend.position = "bottom",
    legend.box.spacing = grid::unit(7, "pt"),
    strip.text = ggplot2::element_text(size = 6)
  )

p_sample <- 
  ggpubr::ggarrange(
    p_counts, p_props, 
    ncol = 1,
    heights = c(1.1, 1),
    labels = "AUTO",
    font.label = list(size = 11, face = "bold")
  )

save_ggplot(
  plot = p_sample,
  path = here::here("inst/visualisation/paper/fig_sample_description.pdf"),
  ncol = 2,
  height = 100,
  return = TRUE
)
