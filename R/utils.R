#' Check that an unquoted column name exists in a data frame
#' @param x A data frame.
#' @param nm An unquoted column name.
#' @importFrom rlang ensym as_string
check_unquoted_col_name <- function(x, nm) {
    col_name_str <- rlang::as_string(rlang::ensym(nm))

    if (!col_name_str %in% names(x)) {
        stop(paste0("`", col_name_str, "` must be a column in `x`. "), call. = FALSE)
    }
}
