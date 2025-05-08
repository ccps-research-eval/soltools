#' Clean Test Names
#'
#' @description Cleans test names by removing the standards year (e.g., "(23)") and " CAT" from the string.
#'
#' @param x A character vector of test names to be cleaned.
#'
#' @return A character vector of cleaned test names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean_test_names(c("Gr 4 Reading CAT", "EOC Reading (17)", "Gr 8 Math (16) CAT"))
#' # Returns: c("Gr 4 Reading", "EOC Reading", "Gr 8 Math")
#' }
#' @md
clean_test_names <- function(x) {
    stopifnot(
        "`x` must be a character vector" = is.character(x)
    )

    # strip out the standards year
    tmp <- gsub("(.*) \\(\\d{2}\\)", "\\1", x)

    # strip out 'CAT'
    ret <- gsub(" CAT", "", tmp)

    return(ret)
}


#' Standardize Writing Test Names
#'
#' @description Standardizes some column names in a writing test data frame to align with the structure of non-writing test data frames.
#'   Specifically, it renames `performance_level_total_mc_tei_sp` to `performance_level` and
#'   `scaled_score_writing_total` to `test_scaled_score`.  This ensures consistency when
#'   analyzing both writing and non-writing assessment data.
#'
#' @param x A data frame containing writing test data, typically from an SOL student data extract. Ideally one created by [ingest_student_data_extract()] where `writing_extract` is `TRUE`.
#'   It should include the columns `performance_level_total_mc_tei_sp` and `scaled_score_writing_total`.
#'
#' @return A data frame with standardized column names: `performance_level` and `test_scaled_score`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' writing_data <- ingest_student_data_extract("path/to/data.csv", writing_extract = TRUE)
#' standardized_data <- standardize_writing_test_names(writing_data)
#' }
#' @md
standardize_writing_test_names <- function(x) {
    req_nms <- c("performance_level_total_mc_tei_sp", "scaled_score_writing_total")

    stopifnot(
        "`x` must be a data frame" = is.data.frame(x),
        "`x` must contain 'performance_level_total_mc_tei_sp' and 'scaled_score_writing_total' columns" = all(req_nms %in% names(x))
    )

    ret <- x |>
        dplyr::rename(
            performance_level = performance_level_total_mc_tei_sp,
            test_scaled_score = scaled_score_writing_total
        )

    return(ret)
}
