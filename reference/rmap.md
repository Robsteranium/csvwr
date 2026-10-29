# Recursive map

Applies function `.f` to each element in `.x` as per
[`purrr::map`](https://purrr.tidyverse.org/reference/map.html). If the
value of the element is itself a list, then the function is applied to
that in turn. The process is followed recursively until an atomic value
at the leaf nodes of the list is found.

## Usage

``` r
rmap(.x, .f)
```

## Arguments

- .x:

  a list

- .f:

  a function (called with elements of `.x` as the first argument)

## Value

A list
