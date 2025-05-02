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

#' Drop Int Read Write Tests
#'
#' @description Filter a student data extract to remove integrated reading and writing (IRW) tests. This function assumes that test names are defined in a column named "test_name."
#'
#' @param x A dataframe, ideally one created by [ingest_student_data_extract()]
#'
#' @return a dataframe with IRW tests removed
#'
#' @export
#'
#' @examples \dontrun{
#' df_no_irw <- drop_irw(my_data)
#' }
drop_irw <- function(x) {
    stopifnot(
        "`x` must be a data frame" = is.data.frame(x),
        "`test_name` must be a character vector" = is.character(x$test_name)
    )
    ret <- dplyr::filter(x, !grepl("Int Read Write", test_name))
}

#' Drop Failing Retests
#'
#' @description Filter a student data extract to remove failing retests in alignment with VDOE's steps to calculate annual pass rates. This function assumes that retest status is defined in a column named "retest," where "Y" indicates a retest. It also assumes that performance levels are defined in a column named "performance_level."
#'
#' @param x A dataframe, ideally one created by [ingest_student_data_extract()]
#'
#' @return a dataframe with failing retests removed
#'
#' @export
#'
#' @examples \dontrun{
#' df_no_fail_retest <- drop_failing_retests(my_data)
#' }
drop_failing_retests <- function(x) {
    stopifnot(
        "`x` must be a data frame" = is.data.frame(x),
        "`retest` column must be in the data frame" = "retest" %in% colnames(x),
        "`retest` column should contain only 'Y' and NA_character_" = length(setdiff(c("Y", NA_character_), unique(x$retest))) == 0
    )

    x[!(!is.na(x$retest) & x$performance_level %in% c(3, 4, 5)), ]
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
