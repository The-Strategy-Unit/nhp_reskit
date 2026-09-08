# Return a zero-row result that keeps the expected output shape

Used by the `compile_*` functions when filtering leaves no rows.
Returning a correctly shaped zero-row tibble, rather than whatever
partially prepared object happened to be in hand, means the downstream
`make_*` functions can rely on the output columns existing. The
`reskit_no_data` attribute lets those functions render an explicit "no
data" panel rather than a blank chart or a cryptic missing-column error.
This function and its documentation were suggested by an LLM.

## Usage

``` r
empty_result(prototype, reason = NULL)
```

## Arguments

- prototype:

  A zero-row tibble giving the columns and types that the calling
  function returns when data are available

- reason:

  A string explaining why no rows remain, stored as an attribute

## Value

`prototype`, carrying a `reskit_no_data` attribute
