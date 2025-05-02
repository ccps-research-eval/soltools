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

# testing drop_irw func
test_that("drop_irw works", {
  df <- data.frame(
    test_name = c("Math", "Int Read Write", "Science"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(drop_irw(df)), 2)
  expect_true(!"Int Read Write" %in% drop_irw(df)$test_name)
})

# testing drop_failing_retests func
test_that("drop_failing_retests works", {
  df <- data.frame(
    state_testing_identifier_sti = rep(1:3, each = 2),
    test_name = rep(c("Math 8", "Reading 8"), 3),
    performance_level = c(1, 3, 2, 4, 5, 1),
    retest = c(NA_character_, "Y", NA_character_, "Y", NA_character_, "Y")
  )

  expect_identical(
    drop_failing_retests(df),
    df[c(1, 3, 5, 6), ]
  )
})

test_that("drop_failing_retests errors if retest col is missing or malformed", {
  df <- create_pass_rate_test_df()

  expect_error(drop_failing_retests(df))
  expect_error(drop_failing_retests(dplyr::select(df, -retest)))
})
