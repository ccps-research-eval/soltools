test_that("filter_test_performance errors for unsupported option", {
  tmp <- create_test_performance_data()
  expect_error(
    filter_test_performance(tmp, type = "zzzzz")
  )
})

test_that("filter_test_performance 'first' and 'best' produce same number of rows", {
  pth <- test_path("fixtures", "demo_data.csv")
  tmp <- ingest_student_data_extract(pth)

  b <- filter_test_performance(tmp, type = "best")
  f <- filter_test_performance(tmp, type = "first")

  expect_equal(
    nrow(b),
    nrow(f)
  )
})

test_that("filter_test_performance 'first' yields the first score", {
  tmp <- create_test_performance_data()
  ret <- filter_test_performance(tmp, type = "first")

  # hard-coding 450 bc that's what i know the first score is in the demo data
  expect_equal(ret$test_scaled_score, 450)
})

test_that("filter_test_performance 'best' yields the best score", {
  tmp <- create_test_performance_data()
  ret <- filter_test_performance(tmp, type = "best")

  # hard-coding 500 bc that's what i know the best score is in the demo data
  expect_equal(ret$test_scaled_score, 500)
})
