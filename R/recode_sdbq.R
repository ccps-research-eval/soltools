#' Recode SDBQ
#' 
#' @description 
#' This function recodes two columns in the SDBQ data extracts. It recodes \code{response} column to numeric values (1s and 0s), and it recodes the \code{race} column to text rather than codes (e.g. "05" is recoded to "White").
#' 
#' It also provides options to collapse infrequently-occurring races into an "Other" category. You can set the threshold at which races are considered infrequently occuring using the \code{collapse_thresh} argument.
#' 
#' @param x data. SDBQ data extract to be passed in.
#' @param collapse_races logical. If TRUE, will collapse infrequently occuring races into an "Other" category.
#' @param collapse_thresh numeric value between 0 and 1. Races with a rate of occurence less than this will be collapsed into "Other".
#' 
#' @export 
#' 
#' @return
#' A dataframe with recoded values
#' 
#' @examples \dontrun{
#' library(tidyverse)
#' #first read in your sdbq extract from Pearson
#' df <- read_csv("my_sdbq_extract.csv")
#' 
#' #basic version
#' a <- recode_sdbq(df)
#' 
#' #collapsing race codes with rates less than 10% into other
#' b <- recode_sdbq(df, collapse_races = TRUE, collapse_thresh = .1)
#' }

recode_sdbq <- function(x, collapse_races = FALSE, collapse_thresh = .05) {

    #checks
    if(!"data.frame" %in% class(x)) {
        stop("`x` must be a dataframe or tibble")
    }

    if (!is.logical(collapse_races)) {
        stop("`collapse_races` must be either TRUE or FALSE")
    }

    if (collapse_thresh < 0 | collapse_thresh > 1 | !is.numeric(collapse_thresh)) {
        stop("`collapse_thresh` must be a numeric value between 0 and 1")
    }

    tmp <- janitor::clean_names(x)

    #recode response column to binary
    tmp$response <- ifelse(tmp$response == "INC", 0, 1)

    tmp$race <- as.numeric(tmp$race)

    #recode race codes into text
    tmp <- dplyr::mutate(tmp, race = dplyr::case_when(
        .data$ethnicity == "Y" ~ "Hispanic",
        .data$ethnicity == "N" & .data$race == 1 ~ "American Indian or Alaskan Native",
        .data$ethnicity == "N" & .data$race == 2 ~ "Asian",
        .data$ethnicity == "N" & .data$race == 3 ~ "Black or African American",
        .data$ethnicity == "N" & .data$race == 5 ~ "White",
        .data$ethnicity == "N" & .data$race == 6 ~ "Native Hawaiian or Other Pacific Islander",
        .data$ethnicity == "N" & .data$race > 6 ~ "Two or More",
        TRUE ~ NA_character_
    ))

    tmp <- if (collapse_races == TRUE) {
        pcts <- table(tmp$race) / length(df$race)

        others <- names(pcts)[pcts <= collapse_thresh]

        tmp$race <- ifelse(tmp$race %in% others, "Other", tmp$race)

        tmp
    } else {
        tmp
    }

    tmp
}
