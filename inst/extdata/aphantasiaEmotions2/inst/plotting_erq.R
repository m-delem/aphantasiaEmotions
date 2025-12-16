pacman::p_load(ggplot2, mgcv, patchwork)

here::here("R") |> fs::dir_ls() |> purrr::walk(source)

source(here::here("R/ggplot_tools.R"))
source(here::here("R/ggplot_aphantasia.R"))

load(here::here("data/erq_data.rda"))
load(here::here("data/paq_data.rda"))

m_gam_erq_s <- gam(erq_s ~ s(vviq), data = erq_data)
m_gam_erq_r <- gam(erq_r ~ s(vviq), data = erq_data)

ptl <-
  plot_correlation(
    erq_data, "erq_r", 
    coef_coords = c(30, 29), 
    title = "ERQ Cognitive Reappraisal"
  ) +
  scale_discrete_aphantasia()

ptr <-
  plot_correlation(
    erq_data, "erq_s", 
    coef_coords = c(30, 11), 
    title = "ERQ Expressive Suppression"
  ) +
  scale_discrete_aphantasia()

pml <-
  plot_gam_relations(m_gam_erq_r) +
  plot_coloured_subjects(
    df = erq_data,
    x = erq_data$vviq, 
    y = erq_data$erq_r,
    alpha = 0.6,
    size = 1.2
  ) +
  scale_discrete_aphantasia(aesthetics = "colour") +
  gam_style(
    plot.margin = margin(3.5, 10, 3.5, 3.5)
  )

pmr <-
  plot_gam_relations(m_gam_erq_s) +
  plot_coloured_subjects(
    df = erq_data,
    x = erq_data$vviq, 
    y = erq_data$erq_s,
    alpha = 0.6,
    size = 1.2
  ) +
  scale_discrete_aphantasia(aesthetics = "colour") +
  gam_style()

pbl <-
  plot_gam_slopes(m_gam_erq_r) +
  gam_style(
    plot.margin = margin(3.5, 7, 3.5, 3.5)
  )

pbr <-
  plot_gam_slopes(m_gam_erq_s) +
  gam_style()

p_top <- 
  ptl + ptr + 
  plot_annotation(
    title = "VVIQ and ERQ data from D'Argembeau & Van der Linden (2006)",
    subtitle = write_sample_info(erq_data, n_groups = 4),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 9,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 8,
        margin = margin(b = 8)
      )
    )
  ) + 
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(
    legend.key.spacing.x = grid::unit(20, "pt"),
    legend.position = "bottom"
  )

p_middle <-
  pml + pmr + 
  plot_layout(axis_titles = "collect") &
  theme(legend.position = "none")

p_bottom <-
  pbl + pbr +
  plot_layout(axis_titles = "collect")


ggpubr::ggarrange(
  p_top, p_middle, p_bottom,
  ncol = 1,
  heights = c(1.25, 1, 1),
  labels = "AUTO",
  vjust = 1.25
  ) |> 
  save_ggplot(
    path = here::here("inst/figures/vviq_erq.pdf"),
    ncol = 2,
    height = 230,
    return = TRUE
  )
