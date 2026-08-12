# Read a selection of (or all) parquet files in an Azure directory

Read a selection of (or all) parquet files in an Azure directory

## Usage

``` r
read_results_parquet_files(container, path, tables = NULL)
```

## Arguments

- container:

  An Azure container.

- path:

  string. Path to an Azure directory of model results data. Potentially
  pulled from a field in a table of model runs metadata

- tables:

  character vector. `NULL`, the default, results in all available
  parquet files in the `path` folder being read in. If you wish only to
  read in a subset of the files, specify their names here, without the
  ".parquet" file extension

## Value

A named list of tibbles

## Examples

``` r
if (FALSE) { # \dontrun{
  data <- azkit::get_container("data_container") |>
    read_results_parquet_files("data/dev/national/test", "acuity")

  data_list <- azkit::get_container("data_container") |>
    read_results_parquet_files(path = "files/v4.0/national")
} # }
```
