# Data preparation step for 'activity in detail' table

Data preparation step for 'activity in detail' table

## Usage

``` r
prepare_detailed_activity_data(dat, aggregation, pod_lookup)
```

## Arguments

- aggregation:

  string. One of "age_group" or "tretspef_grouped"

- pod_lookup:

  A tibble, or a function that returns a tibble, containing columns
  named `activity_type_label`, `pod` and `pod_label`. This provides
  friendly labels for POD variables in the data. `pod` is the key column
  used for joining to data tables.

## Value

A tibble
