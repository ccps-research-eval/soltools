# Changelog

## v0.1.4

- Set `filter_test_performance("best")` to always return 1 test per student, even in the case of multiple identical test scores

## v0.1.3

- Add support for processing writing test extracts

## v0.1.2

- Fix issue with checks in `filter_exclusions()`

## v0.1.1

- Add `drop_irw()` function to drop integrated reading and writing tests
- Add `drop_failing_retests()` function.
- Export `filter_exclusions()` function
- Fix error in `filter_exclusions()` function to properly exclude recently arrived ELs only for failing Reading retests