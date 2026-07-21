test_that("check_scales_reliability() returns correctly shaped output", {
  skip_if_not_installed("psych")
  
  set.seed(42)
  
  make_group_items <- function(n = 60) {
    # Item-level fake data with realistic correlation structure: each item
    # is a noisy version of a shared latent trait, so the scales have
    # non-degenerate (but not perfect) reliability -- closer to real data
    # than either independent noise or perfectly correlated items, either
    # of which can make psych::omega() error out or warn heavily.
    latent_vviq <- stats::rnorm(n)
    latent_tas  <- stats::rnorm(n)
    
    vviq_items <- purrr::map(
      1:16,
      \(i) latent_vviq + stats::rnorm(n, sd = 0.8)
    ) |> 
      rlang::set_names(paste0("vviq_q", 1:16))
    
    tas_items <- purrr::map(
      1:20,
      \(i) latent_tas + stats::rnorm(n, sd = 0.8)
    ) |> 
      rlang::set_names(paste0("tas_q", 1:20))
    
    tibble::as_tibble(c(vviq_items, tas_items))
  }
  
  fake_data <- tibble::tibble(
    study = c("study_a", "study_b"),
    items = list(make_group_items(), make_group_items())
  )
  
  result <- 
    check_scales_reliability(
      fake_data, 
      study, 
      scales = c("dif", "ddf", "eot"),
      silence = TRUE)
  result <- 
    check_scales_reliability(fake_data, study, silence = FALSE) |> 
    suppressWarnings() |> 
    suppressMessages()
  
  # One row per group per scale (2 studies x 5 scales)
  expect_equal(nrow(result), 10)
  
  expect_named(
    result,
    c("study", "Scale", "Cronbach's alpha", "McDonald's omega")
  )
  
  expect_setequal(
    result$Scale,
    c(
      "VVIQ (16 items)",
      "TAS-20, total (20 items)",
      "TAS-20, DIF (7 items)",
      "TAS-20, DDF (5 items)",
      "TAS-20, EOT (8 items)"
    )
  )
  
  # Reliability coefficients should be within a sane range given the
  # latent-trait-plus-noise structure above (not just "any numeric")
  expect_true(all(result$`Cronbach's alpha` > 0.5))
  expect_true(all(result$`Cronbach's alpha` <= 1))
  expect_true(all(result$`McDonald's omega` > 0.5))
  expect_true(all(result$`McDonald's omega` <= 1))
  
  # digits argument is respected
  result_3d <- check_scales_reliability(
    fake_data, study, digits = 3, silence = TRUE
  )
  max_decimals <- result_3d$`Cronbach's alpha` |>
    as.character() |>
    stringr::str_extract("\\d+") |>
    nchar(allowNA = TRUE) |>
    max(na.rm = TRUE)
  expect_lte(max_decimals, 3)
})
