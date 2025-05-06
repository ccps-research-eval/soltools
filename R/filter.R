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
#' @md
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
#' @md
drop_failing_retests <- function(x) {
    stopifnot(
        "`x` must be a data frame" = is.data.frame(x),
        "`retest` column must be in the data frame" = "retest" %in% colnames(x),
        "`retest` column should contain only 'Y' and NA_character_" = length(setdiff(unique(x$retest), c("Y", NA_character_))) == 0
    )

    x[!(!is.na(x$retest) & x$performance_level %in% c(3, 4, 5)), ]
}


#' Filter Exclusions
#'
#' @description This function filters a student data extract to exclude certain records based on several criteria,
#'   aligning with VDOE's guidelines for calculating annual pass rates.  Specifically, it removes records where:
#'   \itemize{
#'     \item The grade is "TT" (non-enrolled student).
#'     \item The performance level is not in the set of allowed levels (1, 2, 3, 4, 5, 8).
#'     \item The student is a recently arrived English learner (EL) taking a reading test and has a non-passing performance level (3, 4, 5).
#'     \item The test was parent-requested (if `drop_parent_requested` is `TRUE`).
#'     \item The test is a failing retest (if `drop_failing_retests` is `TRUE`).
#'   }
#'
#' @param x A data frame, ideally one created by [ingest_student_data_extract()], containing student test data.
#' @param drop_parent_requested Logical. If `TRUE` (the default), exclude tests marked as parent-requested.
#' @param drop_failing_retests Logical. If `TRUE` (the default), exclude failing retests. See [drop_failing_retests()] for details.
#'
#' @return A filtered data frame with the specified exclusions removed.
#'
#' @md
filter_exclusions <- function(x, drop_parent_requested = TRUE, drop_failing_retests = TRUE) {
    req_nms <- c("grade", "performance_level", "recently_arrived_el", "test_name", "parent_requested", "retest")

    stopifnot(
        "`x` must be a data frame" = is.data.frame(x),
        "`drop_parent_requested` must be a logical" = is.logical(drop_parent_requested),
        "`drop_failing_retests` must be a logical" = is.logical(drop_failing_retests),
        "'grade', 'performance_level', 'recently_arrived_el', 'test_name', 'parent_requested', and 'retest' columns must be in `x`" = all(names(x) %in% req_nms)
    )

    allow_lvls <- c(1, 2, 3, 4, 5, 8)
    non_pass_lvls <- c(3, 4, 5)

    tmp <- x |>
        dplyr::filter(
            grade != "TT",
            performance_level %in% allow_lvls,
            !(!is.na(recently_arrived_el) & grepl("Read", test_name) & performance_level %in% non_pass_lvls)
        )

    tmp <- if (drop_parent_requested) {
        tmp |>
            dplyr::filter(is.na(parent_requested))
    } else {
        tmp
    }

    ret <- if (drop_failing_retests) {
        drop_failing_retests(tmp)
    } else {
        tmp
    }

    return(ret)
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
