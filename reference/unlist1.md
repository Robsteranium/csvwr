# Unlist unless the list-elements are themselves lists

Convert a list of elements to a vector. Unlike
[`base::unlist`](https://rdrr.io/r/base/unlist.html) this doesn't
convert the elements of inner lists to vector elements. Thus only a list
a single layer deep is flattened to a vector.

## Usage

``` r
unlist1(l)
```

## Arguments

- l:

  a list

## Value

A list of lists or a vector
