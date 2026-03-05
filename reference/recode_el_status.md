# Recode English Learner (EL) Status

Recodes a variable representing English Learner (EL) proficiency level
into text categories indicating EL status.

## Usage

``` r
recode_el_status(el_proficiency_lvl_var)
```

## Arguments

- el_proficiency_lvl_var:

  A numeric vector representing EL proficiency level. `NA` values
  typically indicate students who are not currently classified as EL.

## Value

A character vector with "EL" or "Not_EL" values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have a data frame called 'student_data' with an 'el_proficiency' column
student_data <- student_data |>
    dplyr::mutate(
        el_status = recode_el_status(el_proficiency)
    )
} # }
```
