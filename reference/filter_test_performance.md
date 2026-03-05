# Filter Test Performance

Filter a student data extract to include only students' 'best' or
'first' SOL score on a given test.

Note that this is dependent on the test window as well. If the dataframe
passed to the function only contains results from a single testing
window, the function will retain students' first or best test in that
window. If the dataframe includes results from multiple testing windows,
then the function will retain the first or best test across multiple
windows.

## Usage

``` r
filter_test_performance(x, type = "best")
```

## Arguments

- x:

  A dataframe, ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)

- type:

  Character. Either "best" (the default) or "first." "best" will retain
  only each student's best score on a given test, whereas "first" will
  retain each student's chronological first score on a given test. *Note
  that only the "best" option is currently supported for writing tests.*

## Value

a dataframe with 1 row per student per test

## Examples

``` r
if (FALSE) { # \dontrun{
p <- "path/to/my/data.csv"
df <- ingest_student_data_extract(p)
best_tests <- filter_test_performance(df, type = "best")
first_tests <- filter_test_performance(df, type = "first")
} # }
```
