# Read CSV on the Web

If the argument to `filename` is a json metadata document, this will be
used to find csv files for each table using the value of `csvw:url`.

## Usage

``` r
read_csvw(filename, metadata = NULL)
```

## Arguments

- filename:

  a path for a csv table or a json metadata document

- metadata:

  optional user metadata

## Value

csvw metadata list, with a `dataframe` property added to each table

## Details

If the argument to `filename` is a csv file, and no `metadata` is
provided, an attempt is made to derive metadata.

If the argument to `filename` is a csv file, and the `metadata` is
provided, then the given csv will override the value of `csvw:url`.

The csvw metadata is returned as a list. In each table in the table
group, an element named `dataframe` is added which provides the contents
of the csv table parsed into a data frame using the table schema.

## Examples

``` r
if (FALSE) { # \dontrun{
read_csvw("metadata.json")
read_csvw("table.csv", "metadata.json")
} # }
```
