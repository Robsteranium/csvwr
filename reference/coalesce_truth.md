# Coalesce value to truthiness

Determine whether the input is true, with missing values being
interpreted as false.

## Usage

``` r
coalesce_truth(x)
```

## Arguments

- x:

  logical, `NA` or `NULL`

## Value

`FALSE` if x is anything but `TRUE`
