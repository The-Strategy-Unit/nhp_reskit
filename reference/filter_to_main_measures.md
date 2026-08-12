# Filter a table so the `measure` column only contains 6 selected measures

Currently this contains 6 of the 7 possible values; it excludes
"procedures". This function is used in several places in reskit as a
filter.

## Usage

``` r
filter_to_main_measures(tbl)
```

## Arguments

- tbl:

  A tibble
