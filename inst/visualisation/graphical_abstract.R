devtools::load_all()
library(patchwork)

base_size <- 20
ax_titles <- 0.8
ax_rel_x <- 0.65
ax_rel_y <- 0.7

p_groups <-
  plot_group_violins(
    tas ~ vviq_group_4, 
    title = "Categorical model",
    x_lab = "Visual imagery group",
    y_lab = "Alexithymia score",
    dot_size = 0.1,
    middle.linewidth = 0.3,
    base_size = base_size,
    axis_relative_size = 1,
    axis_relative_x = ax_rel_x,
    axis_relative_y = ax_rel_y,
    text = ggplot2::element_text(lineheight = 0.2),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), 
      color = "grey50",
      margin = ggplot2::margin(b = 1)
    ),
    axis.title = 
      ggplot2::element_text(size = ggplot2::rel(ax_titles)),
    axis.title.x = 
      ggplot2::element_text(margin = ggplot2::margin(t = 2)),
    axis.text.x.bottom = 
      ggplot2::element_text(margin = ggplot2::margin(t = 2), color = "black"),
    axis.title.y = 
      ggplot2::element_text(margin = ggplot2::margin(r = 2)),
    axis.text.y.left = 
      ggplot2::element_text(margin = ggplot2::margin(r = 2)),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      color = "grey50", fill = NA, linewidth = 0.1),
  ) + 
  plot_alexithymia_cutoff(
    linewidth = 0.1,
    txt_size = 4,
    txt_x = 1.4, 
    txt_y = 65,
    label = "Clinical\nthreshold",
    lineheight = 0.2
  ) +
  scale_discrete_aphantasia() +
  ggplot2::scale_y_continuous(
    limits = c(18, 96),
    breaks = seq(0, 100, by = 10),
    expand = ggplot2::expansion(
      mult = c(0, 0.03)
    )
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "aphantasia"     = "Aphantasia\nN = 146",
      "hypophantasia"  = "Hypophantasia\nN = 139",
      "typical"        = "Typical\nN = 1101",
      "hyperphantasia" = "Hyperphantasia\nN = 72"
    ),
    expand = ggplot2::expansion(
      mult = 0,
      add  = c(0.4, 0.7)
    )
  )

m_gam <- 
  fit_brms_model(
    formula = tas ~ s(vviq), 
    data = all_data,
    prior = c(brms::prior(normal(0, 20), class = "b")),
    file_refit = "never",
    file = here::here("inst/models/gam_tot.rds")
  )

p_gam <-
  plot_gam_means(
    m_gam,
    title = "Continuous model",
    x_lab = "Visual imagery vividness (16 = no imagery)",
    y_lab = "Alexithymia score",
    base_size = base_size,
    axis_relative_size = 1,
    axis_relative_x = ax_rel_x,
    axis_relative_y = ax_rel_y,
    legend.position = "none",
    text = ggplot2::element_text(lineheight = 0.2),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(ax_titles), 
      color = "grey50",
      margin = ggplot2::margin(b = 1)
    ),
    axis.title = 
      ggplot2::element_text(size = ggplot2::rel(ax_titles)),
    axis.title.x = 
      ggplot2::element_text(margin = ggplot2::margin(t = 2)),
    axis.text.x.bottom = 
      ggplot2::element_text(margin = ggplot2::margin(t = 2), color = "black"),
    axis.title.y = 
      ggplot2::element_text(margin = ggplot2::margin(r = 2)),
    axis.text.y.left = 
      ggplot2::element_text(margin = ggplot2::margin(r = 2)),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      color = "grey50", fill = NA, linewidth = 0.1),
  ) +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas,
    size = 0.1
  ) + 
  plot_alexithymia_cutoff(
    linewidth = 0.1,
    txt_size = 4,
    txt_x = 26,
    txt_y = 65,
    label = "",
    lineheight = 0.2
  ) +
  scale_discrete_aphantasia() +
  scale_x_vviq(breaks = seq(16, 80, by = 8)) +
  ggplot2::scale_y_continuous(
    limits = c(18, 96),
    breaks = seq(0, 100, by = 10),
    expand = ggplot2::expansion(
      mult = c(0, 0.03)
    )
  )

p <- 
  p_groups + plot_spacer() + p_gam +
  plot_layout(
    widths = c(1, 0.001, 1),
    ncol = 3,
    axes = "collect_y", 
    axis_titles = "collect_y"
  ) +
  plot_annotation(
    title = 
      "Alexithymia levels vary non-linearly along the visual imagery spectrum",
    subtitle = paste(
      "People with complete aphantasia break the negative association between",
      "visual imagery vividness\nand emotional processing difficulties,",
      "suggesting alternate strategies of emotion regulation."
    ),
    theme = ggplot2::theme(
      plot.title = 
        ggplot2::element_text(
          size = base_size,
          family = "Montserrat",
          margin = ggplot2::margin(b = 1.5),
          face = "bold",
          hjust = 0
      ),
      plot.subtitle = 
        ggplot2::element_text(
          size = base_size - 4,
          family = "Montserrat",
          margin = ggplot2::margin(b = 3),
          face = "italic",
          hjust = 0,
          lineheight = 0.15
        )
    )
  ) &
  ggplot2::theme(
    plot.margin = ggplot2::margin_auto(1, unit = "pt")
  )

save_ggplot(
  p,
  path = here::here("man/figures/graphical_abstract.png"),
  width = 1200,
  height = 800,
  dpi = 600,
  units = "px",
  return = TRUE
)
