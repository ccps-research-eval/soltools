#' Ingest Student Data Extract
#'
#' @description Ingest an SOL student data extract (csv file). This should be the exact file downloaded from Pearson. This function reads the data in and performs some light cleaning.
#'
#' @param path Character. The path to the file to be ingested.
#' @param clean_test_names Logical. If `TRUE` (the default), test names will be cleaned using [clean_test_names()], which strips out the standard year and the " CAT" suffix where applicable.
#' @param ... Additional arguments passed to [readr::read_csv()]
#'
#' @return A dataframe containing the ingested data.
#'
#' @export
#'
#' @examples \dontrun{
#' p <- "path/to/my/data.csv"
#' df <- ingest_student_data_extract(p)
#' }
#' @md
ingest_student_data_extract <- function(path, clean_test_names = TRUE, ...) {
    # check path
    stopifnot(
        "file does not exist" = file.exists(path),
        "path must be a character" = is.character(path),
        "file must be a csv" = tools::file_ext(path) == "csv"
    )

    tmp <- readr::read_csv(path, ...)

    tmp <- janitor::clean_names(tmp)

    # ensure these are coded as numeric for later functions
    num_vars <- c("test_scaled_score", "race", "performance_level", "disability_status", "el_overall_proficiency_level")

    tmp <- dplyr::mutate(tmp, dplyr::across(num_vars, as.numeric))

    ret <- if (clean_test_names) {
        tmp |>
            dplyr::mutate(test_name = clean_test_names(test_name))
    } else {
        tmp
    }

    return(ret)
}
