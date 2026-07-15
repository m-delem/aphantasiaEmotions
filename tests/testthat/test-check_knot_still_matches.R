test_that("check_knot_still_matches() passes silently when the knot matches", {
  skip_if_not_installed("earth")

  true_knot <- earth::earth(tas ~ vviq, data = all_data)$cuts
  true_knot <- true_knot[, "vviq"]
  true_knot <- unname(true_knot[true_knot != 0][1])

  expect_no_error(
    result <- check_knot_still_matches(all_data, seed_knot = true_knot)
  )
  expect_equal(result, true_knot)
})

test_that("check_knot_still_matches() errors when the knot has moved", {
  skip_if_not_installed("earth")

  expect_error(
    check_knot_still_matches(all_data, seed_knot = -9999),
    "cached segmented model was seeded"
  )
})

test_that("check_knot_still_matches() errors on an unrecognised predictor", {
  skip_if_not_installed("earth")

  expect_error(
    check_knot_still_matches(
      all_data,
      seed_knot = 24,
      formula = tas ~ some_column_that_does_not_exist
    )
  )
})

test_that("check_knot_still_matches() detects a real change in the underlying data", {
  # Directly exercises the scenario the function exists for: data changes,
  # the old seed_knot no longer matches a fresh search.
  skip_if_not_installed("earth")

  true_knot <- earth::earth(tas ~ vviq, data = all_data)$cuts
  true_knot <- true_knot[, "vviq"]
  true_knot <- unname(true_knot[true_knot != 0][1])

  # Perturb the data enough to plausibly shift where earth places its knot:
  # drop the entire floor-VVIQ group, which is exactly the kind of change
  # this check is meant to catch.
  perturbed_data <- all_data[all_data$vviq != 16, ]

  expect_error(
    check_knot_still_matches(perturbed_data, seed_knot = true_knot),
    "cached segmented model was seeded"
  )
})
