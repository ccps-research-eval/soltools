# Ingest Student Data Extract

Ingest an SOL student data extract (csv file). This should be the exact
file downloaded from Pearson. This function reads the data in and
performs some light cleaning.

## Usage

``` r
ingest_student_data_extract(
  path,
  clean_test_names = TRUE,
  writing_extract = FALSE,
  ...
)
```

## Arguments

- path:

  Character. The path to the file to be ingested.

- clean_test_names:

  Logical. If `TRUE` (the default), test names will be cleaned using
  [`clean_test_names()`](https://ccps-research-eval.github.io/soltools/reference/clean_test_names.md),
  which strips out the standard year and the " CAT" suffix where
  applicable.

- writing_extract:

  Logical. If `TRUE`, will assume the file is a student data extract for
  writing tests. If `FALSE` (the default), will assume the file is a
  student data extract for non-writing tests. )

- ...:

  Additional arguments passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

## Value

A dataframe containing the ingested data.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- "path/to/my/data.csv"
df <- ingest_student_data_extract(p)
} # }
```
