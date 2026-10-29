# Map R types to csvw datatype

Translate R types to [csvw
datatypes](https://www.w3.org/TR/tabular-metadata/#datatypes). Acts as
an inverse of `datatype_to_type` but doesn't provide a 1:1
correspondence.

## Usage

``` r
type_to_datatype(types)
```

## Arguments

- types:

  a list of R types

## Value

a list of csvw datatypes
