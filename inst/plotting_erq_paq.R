# pacman allows to check, install and load packages with a single call
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(ggplot2, patchwork, tidyr)

source(here::here("R/ggplot_utils.R"))

load(here::here("data/erq_data.rda"))
load(here::here("data/paq_data.rda"))

p_erq <-
  erq_data |>
  pivot_longer(
    cols = c("erq_r", "erq_s"),
    names_to = "erq_scale",
    values_to = "score"
  ) |> 
  ggplot(
    aes(
      x = vviq,
      y = score,
      color = vviq_group_4
    )
  ) +
  geom_point(alpha = 0.6, size = 1.1) +
  geom_smooth(color = "black", span = 1) +
  facet_wrap(
    vars(erq_scale),
    ncol = 1,
    labeller = as_labeller(
      c(
        "erq_r" = "Cognitive Reappraisal",
        "erq_s" = "Expressive Suppression"
      )
    )
  ) +
  scale_x_continuous(
    breaks = seq(20, 80, by = 4),
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
    title = "Relationship between VVIQ and ERQ sub-scale scores",
    subtitle = write_sample_info(erq_data, n_groups = 4),
    caption = "Data from D'Argembeau & Van der Linden (2006)",
    x = "VVIQ score",
    y = "ERQ sub-scale score"
  ) +
  theme_pdf(
    base_theme = theme_minimal,
    panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
    panel.spacing = grid::unit(8, "pt"),
    plot.title = element_text(size = rel(1)),
    plot.subtitle = element_text(size = rel(0.9)),
    plot.caption = element_text(
      margin = margin(t = 8), 
      color = "grey40"),
    legend.text = element_text(size = rel(0.8))
  )

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

p <- p_erq / p_paq + plot_layout(heights = c(2, 1))

save_ggplot(
  plot = p, 
  path = here::here("inst/figures/vviq_erq_paq.pdf"),
  ncol = 1,
  height = 200,
  show = TRUE
)
