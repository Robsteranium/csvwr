# Create tabular metadata from a list of tables

The table annotations should each be a list with keys for `url` and
`tableSchema`. You can use `derive_table_schema` to derive a schema from
a data frame.

## Usage

``` r
create_metadata(tables)
```

## Arguments

- tables:

  a list of `csvw:table` annotations

## Value

a list describing a tabular metadata annotation

## Examples

``` r
d <- data.frame(foo="bar")
table <- list(url="filename.csv", tableSchema=derive_table_schema(d))
create_metadata(tables=list(table))
#> $`@context`
#> [1] "http://www.w3.org/ns/csvw"
#> 
#> $tables
#> $tables[[1]]
#> $tables[[1]]$url
#> [1] "filename.csv"
#> 
#> $tables[[1]]$tableSchema
#> $tables[[1]]$tableSchema$columns
#>   name titles datatype
#> 1  foo    foo   string
#> 
#> 
#> 
#> 
```
