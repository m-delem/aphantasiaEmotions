# creating this package's sticker
devtools::load_all()

sysfonts::font_add_google("Montserrat")
showtext::showtext_auto()

model_floor <- readRDS(
  "inst/models/floor_group_additive_multilevel_tot.rds"
)
model_data <- model_floor$data

p_base <-
  plot_floor_group(
    model_floor,
    model_data,
    limits = c(16, 82),
    dot_size = 1.7,
    dot_alpha = 0.1,
    floor_jitter_size = 1.7,
    floor_jitter_alpha = 0.1
  ) + 
  labs(caption = NULL) + 
  theme_void() + 
  theme(legend.position = "none")

# Keep only data points
p_base@layers <- list(p_base@layers[[1]], p_base@layers[[10]])

save_ggplot(
  p_base,
  path = here::here("inst/visualisation/sticker/package_sticker_base_v2.jpg"),
  width = 1200,
  height = 1200,
  dpi = 600,
  units = "px",
  return = TRUE
)

base_cropped <- 
  cropcircles::hex_crop(
    images = "inst/visualisation/sticker/package_sticker_base_v2.jpg",
    border_colour = "#394049",
    border_size = 7
  )

p <-
  ggplot2::ggplot() +
  ggpath::geom_from_path(ggplot2::aes(.5, .5, path = base_cropped)) +
  ggplot2::annotate(
    "text",
    x = .04,
    y = .44,
    label = "aphantasia\nEmotions",
    color = "#394049",
    family = "Montserrat",
    fontface = "bold",
    lineheight = 0.25,
    size = 52,
    hjust = 0
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::theme_void()

ggplot2::ggsave(
  filename = "inst/visualisation/sticker/package_sticker_v2.png",
  plot = p,
  width = 5.18,
  height = 6,
  dpi = 300
)

usethis::use_logo(here::here("inst/visualisation/sticker/package_sticker_v2.png"))
pkgdown::build_favicons(overwrite = TRUE)
