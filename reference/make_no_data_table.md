# Render a placeholder table when there are no data to display

Preferred over returning an empty `gt` table, which renders as a bare
set of column headings and gives the reader no clue why it is blank.

## Usage

``` r
make_no_data_table(reason = NULL)
```

## Arguments

- reason:

  A string explaining why there are no data, or `NULL`

## Value

A gt table
