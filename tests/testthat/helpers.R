create_test_performance_data <- function() {
    pth <- test_path("fixtures", "test_performance_fixture.csv")

    readr::read_csv(pth)
}
