# Create a default table schema given a csv file and dialect

If neither the table nor the group have a `tableSchema` annotation, then
this default schema will used.

## Usage

``` r
default_schema(filename, dialect = default_dialect)
```

## Arguments

- filename:

  a csv file

- dialect:

  specification of the csv's dialect (default: `default_dialect`)

## Value

a table schema
