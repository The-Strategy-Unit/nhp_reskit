# Compile data to support the "activity distribution summary" tables

Compile data to support the "activity distribution summary" tables

## Usage

``` r
compile_distribution_plot_data(
  results,
  measure,
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  pod_lookup = get_detailed_pods(),
  sites = NULL
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
