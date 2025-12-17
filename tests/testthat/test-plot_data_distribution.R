test_that("ggplot2 wrappers work properly", {
  df_summary <- summarise_aph_and_alexi(all_data, vviq_group_4)
  p1 <- 
    plot_alexithymia_proportions(
      df_summary, vviq_group_4,
      base_size = 12,
      prop_txt_size = 3
    )
  p2 <- 
    plot_vviq_group_proportions(
      all_data, vviq_group_4,
      base_size = 12, prop_txt_size = 3
    )
  p3 <-
    plot_group_violins(
      tas ~ vviq_group_4,
      y_lab = "TAS Score",
      base_size = 12
    ) +
    scale_x_aphantasia(add = c(0.4, 0.7))
  
  expect_contains(class(p1), c("gg", "ggplot"))
  expect_contains(class(p2), c("gg", "ggplot"))
  expect_contains(class(p3), c("gg", "ggplot"))
})

