# Recode SPED Status

Recodes a variable representing Special Education (SPED) status into
text categories.

## Usage

``` r
recode_sped_status(sped_var)
```

## Arguments

- sped_var:

  A numeric vector representing SPED status. `NA` or 15 typically
  indicates "Not SPED".

## Value

A character vector with "Not_SPED" or "SPED" values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have a data frame called 'student_data' with a 'sped_status' column
student_data <- student_data |>
    dplyr::mutate(
        sped_status_text = recode_sped_status(sped_status)
    )
} # }
```
