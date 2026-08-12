# Prepare a site-level summary of main projection results by PoD and LoS

Intended to be used to create a table to be exported to .csv/.xlsx

## Usage

``` r
export_principal_los_data(
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

A tibble
