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
