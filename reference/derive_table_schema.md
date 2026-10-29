# Derive csvw table schema from a data frame

Derive csvw table schema from a data frame

## Usage

``` r
derive_table_schema(d)
```

## Arguments

- d:

  a data frame

## Value

a list describing a `csvw:tableSchema`

## Examples

``` r
derive_table_schema(data.frame(a=1,b=2))
#> $columns
#>   name titles datatype
#> 1    a      a   number
#> 2    b      b   number
#> 
```
