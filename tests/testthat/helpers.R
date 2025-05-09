create_test_performance_data <- function() {
    pth <- test_path("fixtures", "test_performance_fixture.csv")

    readr::read_csv(pth)
}

# helper to create a test dataframe useful for testing functions in pass_rates.R
create_pass_rate_test_df <- function() {
    data.frame(
        state_testing_identifier_sti = rep(1:3, each = 3),
        school_name = rep(c("School A", "School B", "School C"), 3),
        test_name = rep(c("Math 8", "Reading 8", "Biology"), 3),
        grade = rep(c("08", "08", "09"), 3),
        performance_level = c(1, 2, 3, 4, 5, 8, 9, 1, 2),
        recently_arrived_el = rep(NA_character_, 9),
        parent_requested = c(NA_character_, NA_character_, "Y", NA_character_, NA_character_, NA_character_, NA_character_, "Y", NA_character_),
        retest = NA_character_
    )
}
