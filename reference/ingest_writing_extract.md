# Ingest a Writing Student Data Extract

This is a wrapper around
[`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)
with `writing_extract` set to `TRUE`. It ingests and provides light
cleaning for a student data extract for SOL writing tests. See
[`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md)
for more details on arguments and functionality.

## Usage

``` r
ingest_writing_extract(path, clean_test_names = TRUE, ...)
```

## Arguments

- path:

  Character. The path to the file to be ingested.

- clean_test_names:

  Logical. If `TRUE` (the default), test names will be cleaned using
  [`clean_test_names()`](https://ccps-research-eval.github.io/soltools/reference/clean_test_names.md).

- ...:

  Additional arguments passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).

## Value

A dataframe containing the ingested data.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- "path/to/my/writing_data.csv"
df <- ingest_writing_extract(p)
} # }
```
