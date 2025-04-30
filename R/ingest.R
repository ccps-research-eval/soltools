#' Ingest Student Data Extract
#'
#' @description Ingest an SOL student data extract (csv file). This should be the exact file downloaded from Pearson. This function reads the data in and performs some light cleaning.
#'
#' @param path Character. The path to the file to be ingested.
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
ingest_student_data_extract <- function(path, ...) {
    # check path
    stopifnot(file.exists(path))

    tmp <- readr::read_csv(path, ...)

    tmp <- janitor::clean_names(tmp)

    # ensure these are coded as numeric for later functions
    num_vars <- c("test_scaled_score", "race", "performance_level", "disability_status", "el_overall_proficiency_level")

    tmp <- dplyr::mutate(tmp, dplyr::across(num_vars, as.numeric))

    return(tmp)
}
