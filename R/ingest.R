#' Ingest Student Data Extract
#'
#' @description Ingest an SOL student data extract (csv file). This should be the exact file downloaded from Pearson. This function reads the data in and performs some light cleaning.
#'
#' @param path Character. The path to the file to be ingested.
#' @param clean_test_names Logical. If `TRUE` (the default), test names will be cleaned using [clean_test_names()], which strips out the standard year and the " CAT" suffix where applicable.
#' @param writing_extract Logical. If `TRUE`, will assume the file is a student data extract for writing tests. If `FALSE` (the default), will assume the file is a student data extract for non-writing tests.
#' )
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
ingest_student_data_extract <- function(path, clean_test_names = TRUE, writing_extract = FALSE, ...) {
    # check path
    stopifnot(
        "file does not exist" = file.exists(path),
        "path must be a character" = is.character(path),
        "file must be a csv" = tools::file_ext(path) == "csv",
        "`writing_extract` must be a logical" = is.logical(writing_extract),
        "`clean_test_names` must be a logical" = is.logical(clean_test_names)
    )

    tmp <- readr::read_csv(path, ...)

    tmp <- janitor::clean_names(tmp)

    # ensure these are coded as numeric for later functions
    num_vars <- if (writing_extract) {
        c("scaled_score_writing_total", "race", "performance_level_total_mc_tei_sp", "disability_status", "el_overall_proficiency_level")
    } else {
        c("test_scaled_score", "race", "performance_level", "disability_status", "el_overall_proficiency_level")
    }

    tmp <- dplyr::mutate(tmp, dplyr::across(dplyr::all_of(num_vars), as.numeric))

    ret <- if (clean_test_names) {
        tmp |>
            dplyr::mutate(test_name = clean_test_names(test_name))
    } else {
        tmp
    }

    return(ret)
}
