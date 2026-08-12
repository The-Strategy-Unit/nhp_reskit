# grepv a glued regex

Facilitates using regex in search/filter patterns, and puts the
arguments "the right way round" (x first, then pattern), unlike
[grepv](https://rdrr.io/r/base/grep.html)

## Usage

``` r
gregv(x, rx, g = parent.frame())
```

## Value

A character vector: all values of x that match the regex in rx
