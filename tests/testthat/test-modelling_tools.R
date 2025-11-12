test_that("modelling functions work as expected", {
  expect_contains(class(fit_vviq_gam()), "gam")
  
  m_total <- lm(tas ~ vviq_group_4, data = tas_data)
  expect_contains(class(report_contrast(m_total, ~vviq_group_4)), "emmGrid")
})

test_that("ignore_unused_imports works properly", {
  expect_null(ignore_unused_imports())
})
