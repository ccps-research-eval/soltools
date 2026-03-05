# Drop IRW Tests

Filter a student data extract to remove integrated reading and writing
(IRW) tests. This function assumes that test names are defined in a
column named "test_name."

## Usage

``` r
drop_irw(x)
```

## Arguments

- x:

  A dataframe, ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)

## Value

a dataframe with IRW tests removed

## Examples

``` r
if (FALSE) { # \dontrun{
df_no_irw <- drop_irw(my_data)
} # }
```
