# Render a placeholder plot when there are no data to display

The plot equivalent of
[make_no_data_table](https://the-strategy-unit.github.io/nhp_reskit/reference/make_no_data_table.md).
Preferred over returning an empty ggplot, which renders as a blank panel
and gives the reader no clue why it is blank. Returning a ggplot object
(rather than, say, a table) keeps the return type of the `make_*_plot`
functions consistent, so callers can still pass the result to
`plotly::ggplotly()` or a patchwork layout.

## Usage

``` r
make_no_data_plot(reason = NULL)
```

## Arguments

- reason:

  A string explaining why there are no data, or `NULL`

## Value

A ggplot object
