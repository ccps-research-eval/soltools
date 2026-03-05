# Recode Race and Ethnicity Variables

Recodes race and ethnicity variables into standardized text categories.

## Usage

``` r
recode_race_eth(eth_var = NULL, race_var = NULL)
```

## Arguments

- eth_var:

  A character vector representing ethnicity where "Y" indicates Hispanic
  ethnicity and "N" indicates non-Hispanic ethnicity.

- race_var:

  A numeric vector representing race, where the numeric codes correspond
  to VDOE's race codes.

## Value

A character vector of recoded race/ethnicity values.

## Details

This function uses
[`dplyr::case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
to recode the input variables.

- If `eth_var` is "Y", the output is "Hispanic".

- Otherwise, the function checks `race_var` for the following codes:

  - 1: "American Indian/Alaska Native"

  - 2: "Asian"

  - 3: "Black"

  - 5: "White"

  - 6: "Hawaiian or Pacific Islander"

  - 7 or greater: "Two or More Races"

- If none of the above conditions are met, the function will return NA.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have a data frame called 'student_data' with 'ethnicity' and 'race' columns
student_data <- student_data |>
    dplyr::mutate(
        race_ethnicity = recode_race_eth(ethnicity, race)
    )

# Using different column names
student_data <- student_data |>
    dplyr::mutate(
        race_ethnicity = recode_race_eth(hispanic_flag, race_code)
    )
} # }
```
