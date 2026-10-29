# Recursive lmap

Applies function `.f` to each list-element in `.x` as per
[`purrr::lmap`](https://purrr.tidyverse.org/reference/lmap.html). If the
value of the list-element is itself a list, then the function is applied
to that in turn. The process is followed recursively until an atomic
value at the leaf nodes of the list is found. If `.f` modifies the name,
it is thrown away and replaced by the original name.

## Usage

``` r
rlmap(.x, .f, ...)
```

## Arguments

- .x:

  a list

- .f:

  a function (called with elements of `.x` as the first argument)

- ...:

  further arguments passed to the function `.f`

## Value

A list
