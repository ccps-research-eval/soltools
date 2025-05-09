test_that("summarize_pass_rates works correctly", {
  df <- create_pass_rate_test_df()

  # Test with no grouping variables
  result_no_group <- summarize_pass_rates(df, group_vars = NULL, drop_failing_retests = FALSE)
  expect_equal(nrow(result_no_group), 1)
  expect_equal(result_no_group$n_pass, 4)
  expect_equal(result_no_group$n_total, 6) # 9 total, minus 2 parent requested, minus one with performance level = 9
  expect_equal(result_no_group$pass_rate, 4 / 6)

  # Test with grouping variables
  result_grouped <- summarize_pass_rates(df, group_vars = "test_name", drop_failing_retests = FALSE)
  expect_equal(nrow(result_grouped), 3)
  expect_named(result_grouped, c("test_name", "n_pass", "n_total", "pass_rate"))

  # Check specific values for Math
  math_result <- dplyr::filter(result_grouped, test_name == "Math 8")
  expect_equal(math_result$n_pass, 1)
  expect_equal(math_result$n_total, 2) # 3 total, minus one where performance level = 9
  expect_equal(math_result$pass_rate, 1 / 2)

  # Check specific values for Reading
  reading_result <- dplyr::filter(result_grouped, test_name == "Reading 8")
  expect_equal(reading_result$n_pass, 1)
  expect_equal(reading_result$n_total, 2) # 3 total, minus 1 parent requested
  expect_equal(reading_result$pass_rate, 1 / 2)

  # Test error handling
  expect_error(summarize_pass_rates(1, "test_name"))
  expect_error(summarize_pass_rates(df, 1))
  expect_error(summarize_pass_rates(df, "test_name", "TRUE"))
})

test_that("summarize_performance_levels works correctly", {
  df <- create_pass_rate_test_df()

  # Test with grouping variables and text performance levels
  result_text <- summarize_performance_levels(df, group_vars = "test_name", drop_failing_retests = FALSE)
  expect_equal(nrow(result_text), 6) # 2 dropped bc of parent request, 1 dropped bc performance_level = 9
  expect_named(result_text, c("test_name", "performance_level", "n_performance_level", "n_total", "pct"))

  # Test with grouping variables and numeric performance levels
  result_numeric <- summarize_performance_levels(df, group_vars = "test_name", convert_performance_levels = FALSE, drop_parent_requested = FALSE, drop_failing_retests = FALSE)
  expect_equal(nrow(result_numeric), 8) # only dropped 1 row bc of performance_level = 9
  expect_named(result_numeric, c("test_name", "performance_level", "n_performance_level", "n_total", "pct"))

  # Check specific values for Math and performance level "pass_advanced"
  math_pass_advanced <- dplyr::filter(result_text, test_name == "Math 8", performance_level == "pass_advanced")
  expect_equal(math_pass_advanced$n_performance_level, 1)
  expect_equal(math_pass_advanced$n_total, 2)
  expect_equal(math_pass_advanced$pct, 1 / 2)

  # Test error handling
  expect_error(summarize_performance_levels(1, "test_name"))
  expect_error(summarize_performance_levels(df, 1))
  expect_error(summarize_performance_levels(df, "test_name", "TRUE"))
  expect_error(summarize_performance_levels(df, "test_name", convert_performance_levels = "TRUE"))
})
