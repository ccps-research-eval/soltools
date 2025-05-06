#' Summarize Pass Rates
#'
#' @description Calculate pass rates for a student data extract. This function applies the calculation rules described in the VDOE's annual pass rate calculation document. Retest rules are implicitly applied if the data has been preprocessed via [filter_test_performance()].
#'
#' @param x A dataframe containing data from an SOL student data extract. Ideally this will have been read in via [ingest_student_data_extract()] and preprocessed via [filter_test_performance()]
#' @param group_vars NULL or a character vector specifying the column names to group the data by (e.g., "school_name", "test_name"). If `NULL` (the default), no grouping will occur.
#' @param drop_parent_requested Logical. If `TRUE` (the default), drop any tests that were parent requested.
#' @param drop_failing_retests Logical. If `TRUE` (the default), exclude failing retests. See [drop_failing_retests()] for details.
#'
#'
#' @return A summarized dataframe with pass rates for each group
#'
#' @export
#'
#' @examples \dontrun{
#' x <- ingest_student_data_extract("path/to/data.csv")
#' x <- filter_test_performance(x, type = "best")
#' pr <- summarize_pass_rates(x, group_vars = c("school_name", "test_name"))
#' }
#' @md
summarize_pass_rates <- function(x, group_vars = NULL, drop_parent_requested = TRUE, drop_failing_retests = TRUE) {
    stopifnot(
        "`x` must be a data frrame" = is.data.frame(x),
        "`group_vars` must be a character vector" = is.character(group_vars) | is.null(group_vars),
        "`drop_parent_requested` must be a logical" = is.logical(drop_parent_requested),
        "`drop_failing_retests` must be a logical" = is.logical(drop_failing_retests)
    )

    # filter out exclusions
    tmp <- filter_exclusions(x, drop_parent_requested = drop_parent_requested, drop_failing_retests = drop_failing_retests)

    tmp <- tmp |>
        dplyr::mutate(
            pass = dplyr::if_else(performance_level %in% c(1, 2, 8), 1, 0)
        )

    # group if group_vars is provided
    tmp <- if (is.null(group_vars)) {
        tmp
    } else {
        tmp |>
            dplyr::group_by(!!!rlang::syms(group_vars))
    }

    ret <- tmp |>
        dplyr::summarize(
            n_pass = sum(pass, na.rm = TRUE),
            n_total = dplyr::n(),
            pass_rate = n_pass / n_total
        )

    return(ret)
}

#' Summarize Performance Levels
#'
#' @description Summarizes the distribution of performance levels within groups in a student data extract.
#'   Calculates the count and percentage of students at each performance level for each group.
#'
#' @param x A data frame containing student data, ideally from [ingest_student_data_extract()].
#' @param group_vars NULL or a character vector specifying the column names to group the data by (e.g., "school_name", "test_name"). If `NULL` (the default), no grouping will occur.
#' @param drop_parent_requested Logical. If `TRUE` (the default), exclude tests marked as parent-requested.
#' @param drop_failing_retests Logical. If `TRUE` (the default), exclude failing retests. See [drop_failing_retests()] for details.
#' @param convert_performance_levels Logical. If `TRUE` (the default), convert numeric performance level codes to text labels using [recode_performance_levels()].
#'
#' @return A data frame summarizing performance levels.  It includes the grouping variables,
#'   `performance_level` (either numeric or text), `n_performance_level` (the count of students
#'   at that level within the group), `n_total` (the total number of students in the group),
#'   and `pct` (the percentage of students at that level within the group).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with a hypothetical 'student_data' data frame:
#' summary_df <- summarize_performance_levels(
#'     student_data,
#'     group_vars = c("school_name", "test_name")
#' )
#'
#' # Example with numeric performance levels:
#' summary_df_numeric <- summarize_performance_levels(
#'     student_data,
#'     group_vars = "school_name",
#'     convert_performance_levels = FALSE
#' )
#' }
#' @md
summarize_performance_levels <- function(x, group_vars = NULL, drop_parent_requested = TRUE, drop_failing_retests = TRUE, convert_performance_levels = TRUE) {
    stopifnot(
        "`x` must be a data frrame" = is.data.frame(x),
        "`group_vars` must be NULL or a character vector" = is.character(group_vars) | is.null(group_vars),
        "`drop_parent_requested` must be a logical" = is.logical(drop_parent_requested),
        "`drop_failing_retests` must be a logical" = is.logical(drop_failing_retests),
        "`convert_performance_levels` must be a logical" = is.logical(convert_performance_levels)
    )

    # filter out exclusions
    tmp <- filter_exclusions(x, drop_parent_requested = drop_parent_requested, drop_failing_retests = drop_failing_retests)

    # convert peformance levels to text if specified
    tmp <- if (convert_performance_levels) {
        tmp |>
            dplyr::mutate(
                performance_level = recode_performance_levels(performance_level)
            )
    } else {
        tmp
    }

    tmp <- if (is.null(group_vars)) {
        tmp
    } else {
        tmp |>
            dplyr::group_by(!!!rlang::syms(group_vars))
    }

    ret <- tmp |>
        dplyr::count(performance_level, name = "n_performance_level") |>
        dplyr::add_count(wt = n_performance_level, name = "n_total") |>
        dplyr::mutate(pct = n_performance_level / n_total) |>
        dplyr::ungroup()

    return(ret)
}
