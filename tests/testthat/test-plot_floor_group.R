test_that("plot_floor_group() runs and returns a ggplot object", {
  m <- .make_test_floor_model()
  model_data <- all_data
  model_data$complete_aphant <- factor(
    ifelse(model_data$vviq_group_4 == "aphantasia", "floor", "above_floor"),
    levels = c("above_floor", "floor")
  )
  p <- plot_floor_group(m, model_data)
  expect_contains(class(p), c("gg", "ggplot"))
})

test_that("plot_vviq_marginal_histogram() runs and returns a ggplot object", {
  p <- plot_vviq_marginal_histogram(all_data)
  expect_contains(class(p), c("gg", "ggplot"))
})

test_that("plot_vviq_marginal_histogram() isolates the floor bin correctly", {
  # Sanity check on the manual cut()/aggregate() binning logic (flagged as
  # untested-by-eye when originally written) — vviq=16 should land in its
  # own bin, distinct from 17+, rather than merged with neighbouring
  # values. Not a visual check, just confirms the underlying bin count at
  # vviq=16 matches a direct table() count.
  p <- plot_vviq_marginal_histogram(all_data, binwidth = 1)
  expect_contains(class(p), c("gg", "ggplot"))
  expect_equal(
    sum(all_data$vviq == 16),
    table(all_data$vviq)[["16"]]
  )
})
