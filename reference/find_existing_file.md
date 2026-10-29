# Find the first existing file from a set of candidates

Find the first existing file from a set of candidates

## Usage

``` r
find_existing_file(filenames)
```

## Arguments

- filenames:

  a vector of candidates

## Value

If one of the filenames passed is found, then the first is returned. If
none of the filenames exist, `NULL` is returned
