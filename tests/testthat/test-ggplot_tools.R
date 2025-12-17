test_that("ggplot2 helpers work properly", {
  p <- 
    ggplot2::ggplot(data = all_data) +
    plot_coloured_subjects(
      df = all_data,
      x = all_data$vviq,
      y = all_data$tas,
      alpha = 0.6,
      size = 1.2
    ) +
    plot_alexithymia_cutoff() +
    scale_discrete_aphantasia() +
    scale_x_vviq() +
    theme_pdf()
  
  expect_contains(class(p), c("gg", "ggplot"))
  
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
})

test_that("ignore_unused_imports works properly", {
  expect_null(ignore_unused_imports())
})
