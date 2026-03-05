# Drop Failing Retests

Filter a student data extract to remove failing retests in alignment
with VDOE's steps to calculate annual pass rates. This function assumes
that retest status is defined in a column named "retest," where "Y"
indicates a retest. It also assumes that performance levels are defined
in a column named "performance_level."

## Usage

``` r
drop_failing_retests(x)
```

## Arguments

- x:

  A dataframe, ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)

## Value

a dataframe with failing retests removed

## Examples

``` r
if (FALSE) { # \dontrun{
df_no_fail_retest <- drop_failing_retests(my_data)
} # }
```
