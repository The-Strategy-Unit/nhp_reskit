# Prepare a site-level summary of detailed activity results by PoD and LoS

Intended to be used to create a table to be exported to .csv/.xlsx

## Usage

``` r
export_detailed_activity_data(
  results,
  pod_lookup = get_detailed_pods(),
  tretspef_lookup = get_tretspef_lookup(),
  aggregation = c("age_group", "tretspef_grouped"),
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

- tretspef_lookup:

  A tibble, or a function that returns a tibble, containing a `code`
  column (used as a key for joining to the tretspef table) and a
  `tretspef` column that provides friendly labels for specialties

- aggregation:

  string. One of "age_group" or "tretspef_grouped"

- sites:

  Either `NULL` (the default) or a vector of site codes to filter to.
  `NULL` means don't filter; include all sites present in the data

## Value

A tibble
