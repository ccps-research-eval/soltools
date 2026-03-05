test_that("ingest_student_data_extract works for non-writing extracts", {
  # Create a temporary CSV file for testing
  non_writing_data <-
    "\"Test Name\",\"Test Scaled Score\",\"Race\",\"Performance Level\",\"Disability Status\",\"EL Overall Proficiency Level\"\n\"2022 Grade 3 Reading CAT\",\"550\",\"1\",\"4\",\"10\",\"5\""
  temp_path <- tempfile(fileext = ".csv")
  writeLines(non_writing_data, temp_path)
  on.exit(unlink(temp_path))

  testthat::local_mocked_bindings(
    clean_test_names = function(x) "Grade 3 Reading"
  )

  # Test with default arguments
  df <- ingest_student_data_extract(temp_path)

  expect_s3_class(df, "tbl_df")
  expect_named(
    df,
    c(
      "test_name",
      "test_scaled_score",
      "race",
      "performance_level",
      "disability_status",
      "el_overall_proficiency_level"
    )
  )
  expect_equal(df$test_name, "Grade 3 Reading")
  expect_true(is.numeric(df$test_scaled_score))
  expect_true(is.numeric(df$race))
  expect_true(is.numeric(df$performance_level))

  # Test with clean_test_names = FALSE
  df_no_clean <-
    ingest_student_data_extract(temp_path, clean_test_names = FALSE)
  expect_equal(df_no_clean$test_name, "2022 Grade 3 Reading CAT")
})

test_that("ingest_student_data_extract works for writing extracts", {
  # Create a temporary CSV file for testing
  writing_data <-
    "\"Test Name\",\"Scaled Score Writing Total\",\"Race\",\"Performance Level Total MC TEI SP\",\"Disability Status\",\"EL Overall Proficiency Level\"\n\"2022 Grade 8 Writing\",\"450\",\"2\",\"3\",\"11\",\"4\""
  temp_path <- tempfile(fileext = ".csv")
  writeLines(writing_data, temp_path)
  on.exit(unlink(temp_path))

  testthat::local_mocked_bindings(
    clean_test_names = function(x) "Grade 8 Writing"
  )

  df <- ingest_student_data_extract(temp_path, writing_extract = TRUE)

  expect_s3_class(df, "tbl_df")
  expect_named(
    df,
    c(
      "test_name",
      "scaled_score_writing_total",
      "race",
      "performance_level_total_mc_tei_sp",
      "disability_status",
      "el_overall_proficiency_level"
    )
  )
  expect_equal(df$test_name, "Grade 8 Writing")
  expect_true(is.numeric(df$scaled_score_writing_total))
  expect_true(is.numeric(df$race))
  expect_true(is.numeric(df$performance_level_total_mc_tei_sp))
})

test_that("ingest_student_data_extract throws expected errors", {
  expect_error(
    ingest_student_data_extract("nonexistent/file.csv"),
    "file does not exist"
  )

  # Create a non-csv temp file
  temp_path_txt <- tempfile(fileext = ".txt")
  writeLines("a", temp_path_txt)
  on.exit(unlink(temp_path_txt))
  expect_error(
    ingest_student_data_extract(temp_path_txt),
    "file must be a csv"
  )

  # Create a valid csv temp file for other checks
  temp_path_csv <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", temp_path_csv)
  on.exit(unlink(temp_path_csv))

  expect_error(
    ingest_student_data_extract(temp_path_csv, writing_extract = "TRUE"),
    "`writing_extract` must be a logical"
  )
  expect_error(
    ingest_student_data_extract(temp_path_csv, clean_test_names = "TRUE"),
    "`clean_test_names` must be a logical"
  )
})

test_that("ingest_writing_extract is a valid wrapper that passes arguments correctly", {
  # This function is a simple wrapper, so we just need to test that it calls
  # the underlying function with writing_extract = TRUE. We can mock the
  # underlying function to confirm it's called with the right arguments.

  # Create a variable to capture the arguments passed to the mocked function
  call_args <- NULL

  # Replace ingest_student_data_extract with a function that captures its arguments
  testthat::local_mocked_bindings(
    ingest_student_data_extract = function(...) {
      call_args <<- list(...)
    }
  )

  ingest_writing_extract("path/to/file.csv", clean_test_names = FALSE, some_other_arg = 1)

  # Check that the arguments are what we expect
  expect_equal(call_args, list("path/to/file.csv", clean_test_names = FALSE, writing_extract = TRUE, some_other_arg = 1))
})
