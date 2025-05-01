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
