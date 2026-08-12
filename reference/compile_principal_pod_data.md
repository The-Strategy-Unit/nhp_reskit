# Prepare data from default results table for displaying as summary table

Prepare data from default results table for displaying as summary table

## Usage

``` r
compile_principal_pod_data(
  results,
  pod_lookup = get_principal_pods(),
  sites = NULL
)
```

## Arguments

- results:

  A named list containing NHP results tables

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

- sites:

  Either `NULL` (the default) or a vector of site codes to filter to.
  `NULL` means don't filter; include all sites present in the data

## Value

A filtered and sorted tibble of principal projections of results, by
activity type and point of delivery (PoD)
