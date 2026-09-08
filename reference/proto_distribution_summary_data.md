# Zero-row prototype for the [compile_distribution_summary_data](https://the-strategy-unit.github.io/nhp_reskit/reference/compile_distribution_summary_data.md) output

The column names and types here must match what
[compile_distribution_summary_data](https://the-strategy-unit.github.io/nhp_reskit/reference/compile_distribution_summary_data.md)
returns when rows are present; `test-empty_results.R` asserts this.

## Usage

``` r
proto_distribution_summary_data(value_type = "median")
```

## Value

A zero-row tibble
