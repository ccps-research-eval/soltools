# Filter Exclusions

This function filters a student data extract to exclude certain records
based on several criteria, aligning with VDOE's guidelines for
calculating annual pass rates. Specifically, it removes records where:

- The grade is "TT" (non-enrolled student).

- The performance level is not in the set of allowed levels (1, 2, 3, 4,
  5, 8).

- The student is a recently arrived English learner (EL) taking a
  reading test and has a non-passing performance level (3, 4, 5).

- The test was parent-requested (if `drop_parent_requested` is `TRUE`).

- The test is a failing retest (if `drop_failing_retests` is `TRUE`).

## Usage

``` r
filter_exclusions(x, drop_parent_requested = TRUE, drop_failing_retests = TRUE)
```

## Arguments

- x:

  A data frame, ideally one created by
  [`ingest_student_data_extract()`](https://ccps-research-eval.github.io/soltools/reference/ingest_student_data_extract.md),
  containing student test data.

- drop_parent_requested:

  Logical. If `TRUE` (the default), exclude tests marked as
  parent-requested.

- drop_failing_retests:

  Logical. If `TRUE` (the default), exclude failing retests. See
  [`drop_failing_retests()`](https://ccps-research-eval.github.io/soltools/reference/drop_failing_retests.md)
  for details.

## Value

A filtered data frame with the specified exclusions removed.
