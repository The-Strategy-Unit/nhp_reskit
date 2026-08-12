# From any results table, get list of all site codes for this scheme

The "default" table is recommended

## Usage

``` r
get_trust_sites(res_tbl, col = "sitetret")
```

## Arguments

- res_tbl:

  A tibble from the results list

- col:

  string The name of the column containing site codes. `sitetret` by
  default

## Value

A character vector
