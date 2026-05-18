# Drop Failing High School Retests

Filter a student data extract to remove failing retests for high school
tests. This is a more specific version of
[`drop_failing_retests()`](https://ccps-research-eval.github.io/soltools/reference/drop_failing_retests.md)
that targets only high schools, identified by a pattern in the school
name.

## Usage

``` r
drop_hs_failing_retests(
  x,
  hs_pattern = "HS$|CAREER",
  retest_col = retest,
  performance_lvl_col = performance_level,
  school_col = school_name
)
```

## Arguments

- x:

  A dataframe, ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md).

- hs_pattern:

  A character string containing a regular expression used to identify
  high schools from the school name column. The default is
  `"HS$|CAREER"`.

- retest_col:

  The unquoted column name for the retest indicator. Defaults to
  `retest`.

- performance_lvl_col:

  The unquoted column name for the performance level. Defaults to
  `performance_level`.

- school_col:

  The unquoted column name for the school name. Defaults to
  `school_name`.

## Value

A dataframe with failing high school retests removed.

## Examples

``` r
if (FALSE) { # \dontrun{
my_data <- ingest_student_data_extract("path/to/my/data.csv")
# Remove failing retests from schools with "HS" or "CAREER" in their name
df_no_hs_fail_retest <- drop_hs_failing_retests(my_data)
} # }
```
