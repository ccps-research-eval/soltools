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

#' Recode Race and Ethnicity Variables
#'
#' @description Recodes race and ethnicity variables into standardized text categories.
#'
#' @param eth_var A character vector representing ethnicity where "Y" indicates Hispanic ethnicity and "N" indicates non-Hispanic ethnicity.
#' @param race_var A numeric vector representing race, where the numeric codes correspond to VDOE's race codes.
#'
#' @details
#' This function uses `dplyr::case_when` to recode the input variables.
#' - If `eth_var` is "Y", the output is "Hispanic".
#' - Otherwise, the function checks `race_var` for the following codes:
#'   - 1: "American Indian/Alaska Native"
#'   - 2: "Asian"
#'   - 3: "Black"
#'   - 5: "White"
#'   - 6: "Hawaiian or Pacific Islander"
#'   - 7 or greater: "Two or More Races"
#' - If none of the above conditions are met, the function will return NA.
#'
#' @return A character vector of recoded race/ethnicity values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a data frame called 'student_data' with 'ethnicity' and 'race' columns
#' student_data <- student_data |>
#'     dplyr::mutate(
#'         race_ethnicity = recode_race_eth(ethnicity, race)
#'     )
#'
#' # Using different column names
#' student_data <- student_data |>
#'     dplyr::mutate(
#'         race_ethnicity = recode_race_eth(hispanic_flag, race_code)
#'     )
#' }
#' @md
recode_race_eth <- function(eth_var = NULL, race_var = NULL) {
    stopifnot(
        "`eth_var` must be character" = is.character(eth_var),
        "`race_var` must be numeric" = is.numeric(race_var)
    )

    ret <- dplyr::case_when(
        {{ eth_var }} == "Y" ~ "Hispanic",
        {{ race_var }} == 1 ~ "American Indian/Alaska Native",
        {{ race_var }} == 2 ~ "Asian",
        {{ race_var }} == 3 ~ "Black",
        {{ race_var }} == 5 ~ "White",
        {{ race_var }} == 6 ~ "Hawaiian or Pacific Islander",
        {{ race_var }} >= 7 ~ "Two or More Races",
        .default = NA_character_
    )

    return(ret)
}

#' Recode SPED Status
#'
#' @description Recodes a variable representing Special Education (SPED) status into text categories.
#'
#' @param sped_var A numeric vector representing SPED status.  `NA` or 15 typically indicates "Not SPED".
#'
#' @return A character vector with "Not_SPED" or "SPED" values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a data frame called 'student_data' with a 'sped_status' column
#' student_data <- student_data |>
#'     dplyr::mutate(
#'         sped_status_text = recode_sped_status(sped_status)
#'     )
#' }
#' @md
recode_sped_status <- function(sped_var) {
    stopifnot(
        "`sped_var` must be numeric" = is.numeric(sped_var)
    )

    ret <- dplyr::if_else(is.na(sped_var) | sped_var == 15, "Not_SPED", "SPED")

    return(ret)
}

#' Recode English Learner (EL) Status
#'
#' @description Recodes a variable representing English Learner (EL) proficiency level into text categories indicating EL status.
#'
#' @param el_proficiency_lvl_var A numeric vector representing EL proficiency level. `NA` values typically indicate students who are currently classified as EL.
#'
#' @return A character vector with "EL" or "Not_EL" values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a data frame called 'student_data' with an 'el_proficiency' column
#' student_data <- student_data |>
#'     dplyr::mutate(
#'         el_status = recode_el_status(el_proficiency)
#'     )
#' }
#' @md
recode_el_status <- function(el_proficiency_lvl_var) {
    stopifnot(
        "`el_proficiency_lvl_var must be numeric" = is.numeric(el_proficiency_lvl_var)
    )

    ret <- dplyr::if_else(is.na(el_proficiency_lvl_var), "Not_EL", "EL")

    return(ret)
}

#' Recode Demographic Variables
#'
#' @description Recodes demographic variables (SPED status, EL status, race/ethnicity) in a data frame,
#'   allowing for custom input and output column names.
#'
#' @param x A data frame containing the demographic variables to be recoded. This should be the result of [ingest_student_data_extract()]
#' @param sped_input_var Character. The name of the column containing SPED status codes.
#'   Defaults to "disability_status".
#' @param sped_output_var Character (optional). The name of the column to store the recoded
#'   SPED status. If `NULL` (the default), the recoded values will overwrite the input column.
#' @param el_input_var Character. The name of the column containing EL proficiency level codes.
#'   Defaults to "el_overall_proficiency_level".
#' @param el_output_var Character (optional). The name of the column to store the recoded
#'   EL status. If `NULL` (the default), the recoded values will overwrite the input column.
#' @param race_input_var Character. The name of the column containing race codes.
#'   Defaults to "race".
#' @param ethnicity_input_var Character. The name of the column containing ethnicity codes.
#'   Defaults to "ethnicity".
#' @param race_output_var Character (optional). The name of the column to store the recoded
#'   race/ethnicity. If `NULL` (the default), the recoded values will overwrite the input column.
#'
#' @return A data frame with the specified demographic variables recoded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a data frame called 'student_data'
#'
#' # Recode with default column names:
#' student_data <- recode_demographics(student_data)
#'
#' # Recode with custom output column names:
#' student_data <- recode_demographics(
#'     student_data,
#'     sped_output_var = "sped_status_text",
#'     el_output_var = "el_status_text",
#'     race_output_var = "race_ethnicity_text"
#' )
#'
#' # Recode with different input column names:
#' student_data <- recode_demographics(
#'     student_data,
#'     sped_input_var = "special_ed_code",
#'     el_input_var = "el_prof_level",
#'     race_input_var = "race_code",
#'     ethnicity_input_var = "hispanic_flag",
#'     sped_output_var = "sped_status_text",
#'     el_output_var = "el_status_text",
#'     race_output_var = "race_ethnicity_text"
#' )
#' }
#' @md
recode_demographics <- function(
    x,
    sped_input_var = "disability_status",
    sped_output_var = NULL,
    el_input_var = "el_overall_proficiency_level",
    el_output_var = NULL,
    race_input_var = "race",
    ethnicity_input_var = "ethnicity",
    race_output_var = NULL) {
    stopifnot(
        is.data.frame(x),
        sapply(c(sped_input_var, el_input_var, race_input_var, ethnicity_input_var), is.character),
        sapply(list(sped_output_var, el_output_var, race_output_var), \(y) {
            is.null(y) | is.character(y)
        })
    )

    # define output column names if specified
    sp_out <- if (is.null(sped_output_var)) {
        sped_input_var
    } else {
        sped_output_var
    }

    el_out <- if (is.null(el_output_var)) {
        el_input_var
    } else {
        el_output_var
    }

    race_out <- if (is.null(race_output_var)) {
        race_input_var
    } else {
        race_output_var
    }

    # might be more concise to use a do.call flow here? idk. at least with this it's obvious what's going on
    ret <- dplyr::mutate(
        x,
        !!rlang::sym(sp_out) := recode_sped_status(!!rlang::sym(sped_input_var)),
        !!rlang::sym(el_out) := recode_el_status(!!rlang::sym(el_input_var)),
        !!rlang::sym(race_out) := recode_race_eth(eth_var = !!rlang::sym(ethnicity_input_var), race_var = !!rlang::sym(race_input_var))
    )

    return(ret)
}
