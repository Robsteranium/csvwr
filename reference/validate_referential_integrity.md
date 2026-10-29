# Validate the referential integrity of a csvw table group

Fails if foreign keys aren't found in the referenced tables

## Usage

``` r
validate_referential_integrity(csvw)
```

## Arguments

- csvw:

  the metadata annotation

## Value

a list specifying any foreign key violations
