# Compile data to support the "activity distribution summary" tables

Compile data to support the "activity distribution summary" tables

## Usage

``` r
compile_distribution_summary_data(
  results,
  value_type = c("median", "principal"),
  pod_lookup = get_detailed_pods(),
  sites = NULL
)
```

## Arguments

- results:

  A named list containing NHP results tables

- value_type:

  string Either "median" or "principal"

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

- sites:

  Either `NULL` (the default) or a vector of site codes to filter to.
  `NULL` means don't filter; include all sites present in the data

## Value

A tibble
