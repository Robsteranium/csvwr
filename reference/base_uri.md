# Retrieve the base URI from configuration

Retrieve the base URI from configuration

## Usage

``` r
base_uri()
```

## Value

returns the value of `csvwr_base_uri` option, defaulting to
`example.net`

## Examples

``` r
if (FALSE) { # \dontrun{
base_uri() # returns default

options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/")
base_uri()
} # }
```
