# Get path to csvwr example

The csvwr package includes some example csvw files in it's
`inst/extdata` directory. You can use this function to find them.

## Usage

``` r
csvwr_example(path = NULL)
```

## Arguments

- path:

  The filename. If `NULL`, the example files will be listed.

## Value

either a file path or a vector of filenames

## Details

Inspired by
[`readr::readr_example()`](https://readr.tidyverse.org/reference/readr_example.html)

## Examples

``` r
csvwr_example()
#> [1] "computer-scientists.csv"  "computer-scientists.json"
csvwr_example("computer-scientists.csv")
#> [1] "/home/runner/work/_temp/Library/csvwr/extdata/computer-scientists.csv"
```
