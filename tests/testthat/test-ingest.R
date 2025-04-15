test_that("ingest_student_data_extract works", {
  pth <- test_path("fixtures", "demo_data.csv")
  expect_s3_class(
    ingest_student_data_extract(pth), "data.frame"
  )
})

test_that("ingest_student_data_extract errors for non-existent file", {
  pth <- test_path("fixtures", "zzzzz.csv")
  expect_error(
    ingest_student_data_extract(pth)
  )
})
