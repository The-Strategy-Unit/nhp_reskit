# Data preparation step for `change_factor` data

Data preparation step for `change_factor` data

## Usage

``` r
prepare_principal_cf_data(dat, pod_lookup, tpma_lookup, include_baseline)
```

## Arguments

- dat:

  A tibble

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

- include_baseline:

  Boolean. Whether to include baseline data

## Value

A tibble
