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

test_that("drop_failing_retests errors if retest col is missing", {
  df <- create_pass_rate_test_df()

  expect_error(drop_failing_retests(dplyr::select(df, -retest)))
})

# testing drop_hs_failing_retests func
test_that("drop_hs_failing_retests works as expected", {
  df <- data.frame(
    school_name = c(
      "Test HS", # HS, failing retest -> drop
      "Test HS", # HS, passing retest -> keep
      "Test HS", # HS, failing non-retest -> keep
      "CAREER Center", # Career, failing retest -> drop
      "Middle School", # Not HS, failing retest -> keep
      "Elem School" # Not HS, passing retest -> keep
    ),
    retest = c("Y", "Y", NA_character_, "Y", "Y", "Y"),
    performance_level = c(3, 1, 4, 5, 3, 2),
    stringsAsFactors = FALSE
  )

  # Should drop rows 1 and 4
  result <- drop_hs_failing_retests(df)
  expect_equal(nrow(result), 4)
  expect_equal(result, df[c(2, 3, 5, 6), ], ignore_attr = TRUE)
})

test_that("drop_hs_failing_retests works with custom arguments", {
  df <- data.frame(
    school = c("ACADEMY", "Middle"),
    retake = c("Y", "Y"),
    perf_lvl = c(4, 3),
    stringsAsFactors = FALSE
  )

  # Should drop row 1
  result <- drop_hs_failing_retests(
    df,
    hs_pattern = "ACADEMY",
    retest_col = retake,
    performance_lvl_col = perf_lvl,
    school_col = school
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$school, "Middle")
})

test_that("drop_hs_failing_retests throws expected errors", {
  df <- data.frame(school_name = "Test HS", retest = "Y", performance_level = 3)

  expect_error(drop_hs_failing_retests(list()), "`x` must be a dataframe")
  expect_error(drop_hs_failing_retests(df, hs_pattern = c("a", "b")), "`hs_pattern` must be a length-1 character vector")
  expect_error(
    drop_hs_failing_retests(dplyr::select(df, -retest)),
    "`retest` must be a column in `x`"
  )
  expect_error(
    drop_hs_failing_retests(df, school_col = missing_col),
    "`missing_col` must be a column in `x`"
  )
})

test_that("filter_exclusions works as expected", {
  # Create a sample data frame
  y <- data.frame(
    grade = c("10", "09", "11", "12"),
    performance_level = c(1, 2, 6, 3),
    recently_arrived_el = c(NA_character_, "Y", NA_character_, NA_character_),
    test_name = c("Reading", "Math", "Reading", "Math"),
    parent_requested = c(NA_character_, "Y", NA_character_, NA_character_),
    retest = c(NA_character_, "Y", NA_character_, "Y")
  )

  filtered_df <- filter_exclusions(y)
  expect_equal(nrow(filtered_df), 1)
  expect_equal(filtered_df$grade, "10")
})
