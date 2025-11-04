p_paq <-
  paq_data |> 
  ggplot(
    aes(
      x = vviq,
      y = paq,
      color = vviq_group_4
    )
  ) +
  geom_point(alpha = 0.6, size = 1.1) +
  geom_smooth(color = "black") +
  scale_x_continuous(
    breaks = seq(16, 80, by = 4),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_discrete_manual(
    aesthetics = "color",
    name = NULL,
    labels = c(
      "aphantasia"     = "Aphantasia",
      "hypophantasia"  = "Hypophantasia",
      "typical"        = "Typical",
      "hyperphantasia" = "Hyperphantasia"
    ),
    values = c(
      "aphantasia"     = palette.colors()[1],
      "hypophantasia"  = palette.colors()[2],
      "typical"        = palette.colors()[3],
      "hyperphantasia" = palette.colors()[4]
    )
  ) +
  labs(
    title = "Relationship between VVIQ and PAQ scores",
    subtitle = write_sample_info(paq_data, n_groups = 4),
    caption = "Data from Ji et al. (2025), unpublished",
    x = "VVIQ score",
    y = "PAQ total score"
  ) +
  theme_pdf(
    base_theme = theme_minimal,
    panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
    plot.margin = margin(15, 3.5, 3.5, 3.5),
    plot.title = element_text(size = rel(1)),
    plot.subtitle = element_text(size = rel(0.9)),
    plot.caption = element_text(
      margin = margin(t = 8), 
      color = "grey40"),
    legend.text = element_text(size = rel(0.8))
  )