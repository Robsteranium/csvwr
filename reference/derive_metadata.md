# Derive csvw metadata from a csv file

Derive csvw metadata from a csv file

## Usage

``` r
derive_metadata(filename)
```

## Arguments

- filename:

  a csv file

## Value

a list of csvw metadata

## Examples

``` r
derive_metadata(csvwr_example("computer-scientists.csv"))
#> $`@context`
#> [1] "http://www.w3.org/ns/csvw"
#> 
#> $tables
#> $tables[[1]]
#> $tables[[1]]$url
#> [1] "http://example.net//home/runner/work/_temp/Library/csvwr/extdata/computer-scientists.csv"
#> 
#> $tables[[1]]$tableSchema
#> $tables[[1]]$tableSchema$columns
#>            name        titles datatype
#> 1          Name          Name   string
#> 2 Date Of Birth Date Of Birth     date
#> 
#> 
#> 
#> 
```
