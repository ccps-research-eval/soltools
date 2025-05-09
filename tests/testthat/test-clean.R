test_that("clean_test_names works correctly", {
  expect_equal(
    clean_test_names(c("Gr 4 Reading CAT", "EOC Reading (17)", "Gr 8 Math (16) CAT")),
    c("Gr 4 Reading", "EOC Reading", "Gr 8 Math")
  )

  expect_equal(
    clean_test_names("Algebra I (18) CAT"),
    "Algebra I"
  )

  expect_error(clean_test_names(123))
})

test_that("standardize_writing_extract_names works correctly", {
  # Create a sample data frame
  df <- data.frame(
    performance_level_total_mc_tei_sp = c(1, 2, 3),
    scaled_score_writing_total = c(400, 500, 600)
  )

  # Apply the function
  result <- standardize_writing_extract_names(df)

  # Check the result
  expect_equal(result$performance_level, c(1, 2, 3))
  expect_equal(result$test_scaled_score, c(400, 500, 600))
})
