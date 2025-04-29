#' Filter Test Performance
#'
#' @description Filter a student data extract to include only students' 'best' or 'first' SOL tests.
#'
#' @param x A dataframe, ideally one created by [ingest_student_data_extract()]
#' @param type Character. Either "best" (the default) or "first." "best" will retain only each student's best score on a given test, whereas "first" will retain each student's chronological first score on a given test.
#'
#' @return a dataframe with 1 row per student per test
#'
#' @export
#'
#' @examples \dontrun{
#' p <- "path/to/my/data.csv"
#' df <- ingest_student_data_extract(p)
#' best_tests <- filter_test_performance(df, type = "best")
#' first_tests <- filter_test_performance(df, type = "first")
#' }
#' @md
filter_test_performance <- function(x, type = "best") {
    # checks
    stopifnot(
        is.data.frame(x),
        type %in% c("best", "first")
    )

    if (type == "best") {
        return(filter_best_scores(x))
    } else {
        return(filter_first_scores(x))
    }
}

# utility functions ------------------------

filter_best_scores <- function(x) {
    # TODO -- avoid hard-coding column names?
    tmp <- dplyr::group_by(x, state_testing_identifier_sti, test_name)

    ret <- dplyr::slice_max(tmp, test_scaled_score)

    ret <- dplyr::ungroup(ret)

    return(ret)
}

filter_first_scores <- function(x) {
    x$test_date <- make_test_date(x)

    tmp <- dplyr::group_by(x, state_testing_identifier_sti, test_name)

    ret <- dplyr::slice_min(tmp, test_date)

    ret <- dplyr::ungroup(ret)

    # get rid of test_data col to ensure 'first' and 'best' return the same columns as the df they accept
    ret <- dplyr::select(ret, -test_date)

    return(ret)
}

make_test_date <- function(x) {
    # this isn't the ideal way to set this up, but yolo for now
    tmp <- paste0(x$student_test_year, "-", x$student_test_month, "-", x$student_test_day)

    d <- as.Date(tmp, format = "%Y-%m-%d")

    return(d)
}
