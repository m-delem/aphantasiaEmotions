# creating this package's sticker
devtools::load_all()

sysfonts::font_add_google("Montserrat")
showtext::showtext_auto()

p_base <-
  ggplot2::ggplot() +
  plot_coloured_subjects(
    x = all_data$vviq, 
    y = all_data$tas,
    size = 2,
    alpha = 0.1
  ) + 
  scale_discrete_aphantasia() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")

save_ggplot(
  p_base,
  path = here::here("inst/visualisation/package_sticker_base.jpg"),
  width = 1200,
  height = 1200,
  dpi = 600,
  units = "px",
  return = TRUE
)

base_cropped <- 
  cropcircles::hex_crop(
    images = "inst/visualisation/package_sticker_base.jpg",
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
  filename = "inst/visualisation/package_sticker.png",
  plot = p,
  width = 5.18,
  height = 6,
  dpi = 300
)

usethis::use_logo(here::here("inst/visualisation/package_sticker.png"))
pkgdown::build_favicons()
