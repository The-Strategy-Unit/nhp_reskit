# Prepare data from the 'sex+tretspef_grouped' results table

Prepare data from the 'sex+tretspef_grouped' results table

## Usage

``` r
prepare_tretspef_data(results, tretspef_lookup)
```

## Arguments

- results:

  A named list containing NHP results tables

- tretspef_lookup:

  A tibble, or a function that returns a tibble, containing a `code`
  column (used as a key for joining to the tretspef table) and a
  `tretspef` column that provides friendly labels for specialties
