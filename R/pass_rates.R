#' Calculate Pass Rates
#'
#' @description Calculate pass rates for a student data extract. This function applies the calculation rules described in the VDOE's annual pass rate calculation document. Retest rules are implicitly applied if the data has been preprocessed via [filter_test_performance()].
#'
#' @param x A dataframe containing data from an SOL student data extract. Ideally this will have been read in via [ingest_student_data_extract()] and preprocessed via [filter_test_performance()]
#' @param group_vars A character vector of column names to group by.
#' @param drop_parent_requested Logical. If `TRUE` (the default), drop any tests that were parent requested.
#'
#' @return
#'
#' @export
#'
#' @examples \dontrun{
#' x <- ingest_student_data_extract("path/to/data.csv")
#' x <- filter_test_performance(x, type = "best")
#' pr <- calc_pass_rate(x, group_vars = c("school_name", "test_name"))
#' }
#' @md
calc_pass_rate <- function(x, group_vars, drop_parent_requested = TRUE) {
    stopifnot(
        is.data.frame(x),
        is.character(group_vars),
        is.logical(drop_parent_requested)
    )

    # filter out exclusions
    # note -- if we select the student's 'best' or 'first' tests, we don't need to apply any of the rules for retests
    tmp <- x |>
        dplyr::filter(
            grade != "TT",
            performance_level %in% c(1, 2, 3, 4, 5, 8),
            is.na(recently_arrived_el)
        )

    tmp <- if (drop_parent_requested) {
        tmp |>
            dplyr::filter(tmp, is.na(parent_requested))
    } else {
        tmp
    }

    tmp <- tmp |>
        dplyr::mutate(
            pass = dplyr::if_else(performance_level %in% c(1, 2, 8), 1, 0)
        )

    grouped_df <- tmp |>
        dplyr::group_by(!!!rlang::syms(group_vars))

    ret <- grouped_df |>
        dplyr::summarize(
            n_pass = sum(pass),
            n_total = n(),
            pass_rate = n_pass / n_total
        )

    return(ret)
}
