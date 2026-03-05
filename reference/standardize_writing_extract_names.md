# Standardize Writing Extract Names

Standardizes some column names in a writing test data frame to align
with the structure of non-writing test data frames. Specifically, it
renames `performance_level_total_mc_tei_sp` to `performance_level` and
`scaled_score_writing_total` to `test_scaled_score`. This ensures
consistency when analyzing both writing and non-writing assessment data.

## Usage

``` r
standardize_writing_extract_names(x)
```

## Arguments

- x:

  A data frame containing writing test data, typically from an SOL
  student data extract. Ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)
  where `writing_extract` is `TRUE`. It should include the columns
  `performance_level_total_mc_tei_sp` and `scaled_score_writing_total`.

## Value

A data frame with standardized column names: `performance_level` and
`test_scaled_score`.

## Examples

``` r
if (FALSE) { # \dontrun{
writing_data <- ingest_student_data_extract("path/to/data.csv", writing_extract = TRUE)
standardized_data <- standardize_writing_extract_names(writing_data)
} # }
```
