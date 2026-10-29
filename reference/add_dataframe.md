# Add data frame to csvw table annotation

Add data frame to csvw table annotation

## Usage

``` r
add_dataframe(table, filename, group)
```

## Arguments

- table:

  a `csvw:Table` annotation

- filename:

  a filename/ URL for the csv table

- group:

  a list of metadata for the table group to use as a fallback

## Value

a table annotation with a `dataframe` attribute added with data frame
holding the contents of the table
