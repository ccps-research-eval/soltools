#' Recode Performance Levels
#'
#' @description Recode performance levels from numeric codes to text representations
#'
#' @param x Numeric vector of performance level codes
#'
#' @return A character vector of performance levels
#'
#' @export
#'
#' @examples \dontrun{
#' # ...ingest and clean data previously
#' res <- my_sol_data |>
#'     dplyr::mutate(text_performance_level = recode_performance_levels(performance_level))
#' }
#' @md
recode_performance_levels <- function(x) {
    # check x is a numeric vector
    stopifnot(
        is.numeric(x)
    )

    ret <- dplyr::case_when(
        x == 1 ~ "pass_advanced",
        x == 2 ~ "pass_proficient",
        x == 3 ~ "fail",
        x == 4 ~ "fail_basic",
        x == 5 ~ "fail_below_basic",
        x == 8 ~ "pass_advanced",
        x == 9 ~ "no_score",
        .default = NA_character_
    )

    return(ret)
}
