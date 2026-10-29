# Map csvw datatypes to R types

Translate [csvw
datatypes](https://www.w3.org/TR/tabular-metadata/#datatypes) to R
types. This implementation currently targets
[readr::cols](https://readr.tidyverse.org/reference/cols.html) column
specifications.

## Usage

``` r
datatype_to_type(datatypes)
```

## Arguments

- datatypes:

  a list of csvw datatypes

## Value

a [`readr::cols`](https://readr.tidyverse.org/reference/cols.html)
specification - a list of collectors

## Examples

``` r
if (FALSE) { # \dontrun{
cspec <- datatype_to_type(list("double", list(base="date", format="yyyy-MM-dd")))
readr::read_csv(readr::readr_example("challenge.csv"), col_types=cspec)
} # }
```
