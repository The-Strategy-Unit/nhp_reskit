# Generate a cumulative distribution function (ecdf) chart of model run values

Generate a cumulative distribution function (ecdf) chart of model run
values

## Usage

``` r
make_cumulative_distrib_plot(distrib_plot_data, show_zero = FALSE)
```

## Arguments

- distrib_plot_data:

  data frame. As produced by
  [compile_distribution_plot_data](https://the-strategy-unit.github.io/nhp_reskit/reference/compile_distribution_plot_data.md)

- show_zero:

  Boolean. Whether to extend the chart to show a zero value, for
  context.
