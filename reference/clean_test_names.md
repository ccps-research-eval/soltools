# Clean Test Names

Cleans test names by removing the standards year (e.g., "(23)") and "
CAT" from the string.

## Usage

``` r
clean_test_names(x)
```

## Arguments

- x:

  A character vector of test names to be cleaned.

## Value

A character vector of cleaned test names.

## Examples

``` r
if (FALSE) { # \dontrun{
clean_test_names(c("Gr 4 Reading CAT", "EOC Reading (17)", "Gr 8 Math (16) CAT"))
# Returns: c("Gr 4 Reading", "EOC Reading", "Gr 8 Math")
} # }
```
