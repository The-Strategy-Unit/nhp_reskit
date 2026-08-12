# Prepare data from `step_counts` results table for display as charts

Prepare data from `step_counts` results table for display as charts

## Usage

``` r
compile_indiv_change_factor_data(
  results,
  measure,
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL,
  pod_lookup = get_principal_pods(),
  tpma_lookup = get_tpma_label_lookup(),
  sort_by = c("value", "tpma_label")
)
```

## Arguments

- results:

  A named list containing NHP results tables

- measure:

  The measure to focus on for the output table. Valid values depend on
  which activity_type is selected

- activity_type:

  string. One of "ip", "op", "aae". "ip" is the default.

- pods:

  character vector. PoD labels to filter data to. The default value of
  `NULL` means no PoDs will be filtered out

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

- sort_by:

  string, one of "value" or "tpma_label". The former sorts the output
  table by the value of the change, the latter alphabetically by the
  TPMA label

## Value

A prepared tibble of projected negative changes in activity, by TPMA
