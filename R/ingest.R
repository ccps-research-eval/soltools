#' Ingest Student Data Extract
#'
#' @description Ingest an SOL student data extract (csv file). This should be the exact file downloaded from Pearson
#'
#' @param path Character. The path to the file to be ingested.
#' @param ... Additional arguments passed to [readr::read_csv()]
#'
#' @return A dataframe containing the ingested data.
#'
#' @export
#'
#' @examples
#' @md
ingest_student_data_extract <- function(path, ...) {
    # check path
    stopifnot(file.exists(path))

    tmp <- readr::read_csv(path, ...)

    tmp <- janitor::clean_names(tmp)

    tmp$test_scaled_score <- as.numeric(tmp$test_scaled_score)

    return(tmp)
}
