library(superb)

test_that("ggplot2 helpers work properly", {
  p <- 
    ggplot2::ggplot(data = tas_data) +
    plot_coloured_subjects(
      df = tas_data,
      x = tas_data$vviq,
      y = tas_data$tas,
      alpha = 0.6,
      size = 1.2
    ) +
    plot_alexithymia_cutoff() +
    scale_discrete_aphantasia() +
    ggplot2::labs(
      title    = write_sample_info(tas_data, n_groups = 0),
      subtitle = write_sample_info(tas_data, n_groups = 2),
      caption  = write_sample_info(tas_data, n_groups = 3),
      x        = write_sample_info(tas_data, n_groups = 4)
    ) +
    theme_pdf()
  
  expect_error(save_ggplot(p, ncol = 3)) # 1 or 2 max
  expect_error(save_ggplot(p)) # path missing
  
  save_path <- withr::local_tempfile(fileext = ".pdf")
  expect_invisible(
    save_ggplot(p, path = save_path, ncol = 1, verbose = TRUE) |>
      suppressMessages()
  )
  expect_contains(
    class(
      save_ggplot(p, save_path, 2, return = TRUE, verbose = FALSE)
    ),
    c("gg", "ggplot")
  )
  expect_contains(
    class(
      save_ggplot(p, save_path, width = 90, return = TRUE, verbose = FALSE)
    ),
    c("gg", "ggplot")
  )
  
  # Group plots
  p_group <- 
    superb_with_params(tas ~ vviq_group_4, data = tas_data) +
    add_significance(
      size_star = 4,
      tibble::tibble(
        x_star = 1.5,
        y_star = 105,
        stars  = "***",
        x_line = 1,
        x_line_end = 2,
        y_line = .data$y_star - 1.5
      )
    ) +
    scale_x_aphantasia()
  
  p_group <- 
    p_group |> 
    fix_superb_aes() |> 
    customise_superb_plot() |> 
    suppressMessages()
  
  # Correlation plot
  p_corr <- plot_correlation("tas", nl_model = TRUE) |> suppressWarnings()
  
  # GAM plots
  family <- gaussian(link = "identity")
  m_gam <- 
    mgcv::gam(tas ~ s(vviq, bs = "tp"), data = tas_data, family = family)
  
  pm <-
    plot_gam_means(
      m_gam,
      y_title = "Total TAS score"
    ) +
    gam_style()
  
  pr <- plot_gam_relations(m_gam)
  ps <- plot_gam_slopes(m_gam)
  
  expect_contains(class(p_group), c("gg", "ggplot"))
  expect_contains(class(p_corr), c("gg", "ggplot"))
  expect_contains(class(pm), c("gg", "ggplot"))
  expect_contains(class(pr), c("gg", "ggplot"))
  expect_contains(class(ps), c("gg", "ggplot"))
  expect_error(write_sample_info(tas_data, n_groups = 5))
})
