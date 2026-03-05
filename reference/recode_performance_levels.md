# Recode Performance Levels

Recode performance levels from numeric codes to text representations

## Usage

``` r
recode_performance_levels(x)
```

## Arguments

- x:

  Numeric vector of performance level codes

## Value

A character vector of performance levels

## Examples

``` r
if (FALSE) { # \dontrun{
# ...ingest and clean data previously
res <- my_sol_data |>
    dplyr::mutate(text_performance_level = recode_performance_levels(performance_level))
} # }
```
