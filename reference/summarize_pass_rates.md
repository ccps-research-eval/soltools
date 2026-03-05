# Summarize Pass Rates

Calculate pass rates for a student data extract. This function applies
the calculation rules described in the VDOE's annual pass rate
calculation document. Retest rules are implicitly applied if the data
has been preprocessed via
[`filter_test_performance()`](https://ccps-research-eval.github.io/soltools/reference/filter_test_performance.md).

## Usage

``` r
summarize_pass_rates(
  x,
  group_vars = NULL,
  drop_parent_requested = TRUE,
  drop_failing_retests = TRUE
)
```

## Arguments

- x:

  A dataframe containing data from an SOL student data extract. Ideally
  this will have been read in via
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)
  and preprocessed via
  [`filter_test_performance()`](https://ccps-research-eval.github.io/soltools/reference/filter_test_performance.md)

- group_vars:

  NULL or a character vector specifying the column names to group the
  data by (e.g., "school_name", "test_name"). If `NULL` (the default),
  no grouping will occur.

- drop_parent_requested:

  Logical. If `TRUE` (the default), drop any tests that were parent
  requested.

- drop_failing_retests:

  Logical. If `TRUE` (the default), exclude failing retests. See
  [`drop_failing_retests()`](https://ccps-research-eval.github.io/soltools/reference/drop_failing_retests.md)
  for details.

## Value

A summarized dataframe with pass rates for each group

## Examples

``` r
if (FALSE) { # \dontrun{
x <- ingest_student_data_extract("path/to/data.csv")
x <- filter_test_performance(x, type = "best")
pr <- summarize_pass_rates(x, group_vars = c("school_name", "test_name"))
} # }
```
