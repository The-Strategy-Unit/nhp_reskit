# Filter a table so the `measure` column only contains 6 selected measures

Currently this contains 6 of 7 possible values in principal data; it
excludes "procedures". ("arrivals" is found in the step counts file).
This function is used in several places in reskit as a filter.

## Usage

``` r
filter_to_main_measures(tbl)
```

## Arguments

- tbl:

  A tibble
