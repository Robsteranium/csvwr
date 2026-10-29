# Read a data frame from the first table in a csvw

Wrapper around `read_csvw` convenient when you're only interested in the
data and there's only one table

## Usage

``` r
read_csvw_dataframe(filename, metadata = NULL)
```

## Arguments

- filename:

  a path for a csv table or a json metadata document

- metadata:

  optional user metadata

## Value

A data frame parsed using the table schema
