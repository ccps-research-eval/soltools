# Summarize Performance Levels

Summarizes the distribution of performance levels within groups in a
student data extract. Calculates the count and percentage of students at
each performance level for each group.

## Usage

``` r
summarize_performance_levels(
  x,
  group_vars = NULL,
  drop_parent_requested = TRUE,
  drop_failing_retests = TRUE,
  convert_performance_levels = TRUE
)
```

## Arguments

- x:

  A data frame containing student data, ideally from
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md).

- group_vars:

  NULL or a character vector specifying the column names to group the
  data by (e.g., "school_name", "test_name"). If `NULL` (the default),
  no grouping will occur.

- drop_parent_requested:

  Logical. If `TRUE` (the default), exclude tests marked as
  parent-requested.

- drop_failing_retests:

  Logical. If `TRUE` (the default), exclude failing retests. See
  [`drop_failing_retests()`](https://ccps-research-eval.github.io/soltools/reference/drop_failing_retests.md)
  for details.

- convert_performance_levels:

  Logical. If `TRUE` (the default), convert numeric performance level
  codes to text labels using
  [`recode_performance_levels()`](https://ccps-research-eval.github.io/soltools/reference/recode_performance_levels.md).

## Value

A data frame summarizing performance levels. It includes the grouping
variables, `performance_level` (either numeric or text),
`n_performance_level` (the count of students at that level within the
group), `n_total` (the total number of students in the group), and `pct`
(the percentage of students at that level within the group).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage with a hypothetical 'student_data' data frame:
summary_df <- summarize_performance_levels(
    student_data,
    group_vars = c("school_name", "test_name")
)

# Example with numeric performance levels:
summary_df_numeric <- summarize_performance_levels(
    student_data,
    group_vars = "school_name",
    convert_performance_levels = FALSE
)
} # }
```
