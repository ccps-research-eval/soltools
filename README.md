
# soltools

<!-- badges: start -->
![GitHub last commit](https://img.shields.io/github/last-commit/ccps-research-eval/soltools)

[![R-CMD-check](https://github.com/ccps-research-eval/soltools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ccps-research-eval/soltools/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ccps-research-eval/soltools/graph/badge.svg)](https://app.codecov.io/gh/ccps-research-eval/soltools)
<!-- badges: end -->

soltools provides functionality for ingesting and working with Virginia Standard of Learning (SOL) data extracts from Pearson.

It currently only contains functions for working with the non-writing student data extracts, but functionality for working with other extracts, such as student detail by question (SDBQ) files, will be coming soon.

## Installation

You can install the development version of soltools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ccps-research-eval/soltools")
```

## Usage

The primary functionality of `soltools` in the current version revolves around ingesting student data extracts and calculating summary statistics.

To ingest a student data extract from Pearson, you can simply run:
```r
library(soltools)

my_extract <- ingest_student_data_extract("path/to/my/extract.csv")
```

After ingesting the extract, the most likely workflow involves recoding demographic variables (e.g. special education designation, race/ethnicity) into human-readable values, filtering tests according to some rule (i.e. selecting either a student's first or best attempt per test taken), then calculating summary statistics.

```r
extract_cleaned <- my_extract |>
  drop_irw() |> # drops integrated reading and writing tests
  filter_test_performance("best") |> #retains each student's best attempt per test
  recode_demographics() # see ?recode_demographics() for options, but the defaults should be acceptable 

```

With this cleaned dataframe, we might want to calculate pass rates. We can do this using the `summarize_pass_rates()` function, which also allows us to specify different grouping variables.

```r
test_pass_rates <- summarize_pass_rates(extract_cleaned, group_vars = "test_name") # get the pass rate for each test

sch_test_pass_rates <- summarize_pass_rates(extract_cleaned, group_vars = c("school_name", "test_name")) # get the pass rate for each test by school

```

We can similarly calculate performance by level (e.g. "Pass/Advanced," "Pass/Proficient," etc.)

```r
test_performance_by_level <- summarize_performance_levels(extract_cleaned, group_vars = "test_name")

sch_test_performance_by_level <- summarize_performance_levels(extract_cleaned, group_vars = c("school_name", "test_name"))

```

### Writing Extracts

Working with writing extracts only requires a few small changes. To read in a writing student data extract, you can simply set `writing_extract = TRUE` when calling `ingest_student_data_extract()`.

```r
wrtg_extract <- ingest_student_data_extract("path/to/my/writing_extract.csv", writing_extract = TRUE)
```

You should then call the `standardize_writing_extract_names()` function to standardize the column names in the writing extract and ensure they're the same as those in the non-writing extracts.

```r
wrtg_extract <- standardize_writing_extract_names(wrtg_extract)
```

From this point, the process for summarizing pass rates of performance levels will be the same:

```r
wrtg_test_pass_rates <- wrtg_extract |>
  filter_test_performance("best") |> #note that the "first" test option does not currently work for writing extracts
  recode_demographics() |>
  summarize_pass_rates(group_vars = c("school_name", "test_name"))

```
