# Prepare a site-level summary table of change_factor results

Intended to be used to create a table to be exported to .csv/.xlsx

## Usage

``` r
export_principal_cf_data(
  results,
  sites = NULL,
  pod_lookup = get_principal_pods(),
  tpma_lookup = get_tpma_label_lookup()
)
```

## Arguments

- results:

  A named list containing NHP results tables

- sites:

  Either `NULL` (the default) or a vector of site codes to filter to.
  `NULL` means don't filter; include all sites present in the data

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

- tpma_lookup:

  A tibble, or a function that returns a tibble, containing a column
  named `strategy` (used as a key for joining to the `step_counts`
  table) and a column named `tpma_label` that provides friendly labels
  for all TPMAs/strategies

## Value

A tibble
