devtools::load_all()
pacman::p_load(dplyr, ggplot2, patchwork)

p_counts <-
  all_data |>
  bind_rows(all_data |> mutate(study = "total")) |>
  mutate(
    study = factor(
      study, 
      levels = c("burns", "monzel", "mas", "ruby", "kvamme", "total")
    )
  ) |> 
  plot_vviq_group_proportions(vviq_group_4)

p_props <-
  all_data |>  
  summarise_aph_and_alexi(vviq_group_4) |> 
  plot_alexithymia_proportions(vviq_group_4, ncol = 6)

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
