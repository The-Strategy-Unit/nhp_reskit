# Prepare data from the `sex+age_group` or `sex+tretspef_grouped` table

Prepare data from the `sex+age_group` or `sex+tretspef_grouped` table

## Usage

``` r
compile_detailed_activity_data(
  results,
  measure,
  pod_lookup = get_detailed_pods(),
  tretspef_lookup = get_tretspef_lookup(),
  activity_type = c("ip", "op", "aae"),
  aggregation = c("age_group", "tretspef_grouped"),
  pods = NULL,
  sites = NULL
)
```

## Arguments

- results:

  A named list containing NHP results tables

- measure:

  The measure to focus on for the output table. Valid values depend on
  which activity_type is selected

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

- tretspef_lookup:

  A tibble, or a function that returns a tibble, containing a `code`
  column (used as a key for joining to the tretspef table) and a
  `tretspef` column that provides friendly labels for specialties

- activity_type:

  string. One of "ip", "op", "aae". "ip" is the default.

- aggregation:

  string. One of "age_group" or "tretspef_grouped"

- pods:

  character vector. PoD labels to filter data to. The default value of
  `NULL` means no PoDs will be filtered out

- sites:

  Either `NULL` (the default) or a vector of site codes to filter to.
  `NULL` means don't filter; include all sites present in the data
