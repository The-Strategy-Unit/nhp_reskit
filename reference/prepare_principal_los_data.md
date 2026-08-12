# Preparation of site-level data for the main LoS summary table

Preparation of site-level data for the main LoS summary table

## Usage

``` r
prepare_principal_los_data(dat, pod_lookup)
```

## Arguments

- dat:

  A tibble

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

## Value

A tibble
