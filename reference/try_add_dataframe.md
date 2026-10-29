# Try to add a dataframe to the table

If this fails, a list describing the error is added instead

## Usage

``` r
try_add_dataframe(table, ...)
```

## Arguments

- table:

  a `csvw:Table` annotation

- ...:

  arguments to `add_dataframe`

## Value

A table annotation with a `dataframe` attribute added with data frame
holding the contents of the table or an error.
