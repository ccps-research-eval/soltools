# Changelog

## v0.1.4

- Set `filter_test_performance("best")` to always return 1 test per
  student, even in the case of multiple identical test scores

## v0.1.3

- Add support for processing writing test extracts

## v0.1.2

- Fix issue with checks in
  [`filter_exclusions()`](https://ccps-research-eval.github.io/soltools/reference/filter_exclusions.md)

## v0.1.1

- Add
  [`drop_irw()`](https://ccps-research-eval.github.io/soltools/reference/drop_irw.md)
  function to drop integrated reading and writing tests
- Add
  [`drop_failing_retests()`](https://ccps-research-eval.github.io/soltools/reference/drop_failing_retests.md)
  function.
- Export
  [`filter_exclusions()`](https://ccps-research-eval.github.io/soltools/reference/filter_exclusions.md)
  function
- Fix error in
  [`filter_exclusions()`](https://ccps-research-eval.github.io/soltools/reference/filter_exclusions.md)
  function to properly exclude recently arrived ELs only for failing
  Reading retests
