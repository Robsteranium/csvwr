# Parse metadata

Coerces the metadata to ensure it describes a table group. Retrieves any
linked tableSchema.

## Usage

``` r
parse_metadata(metadata, location)
```

## Arguments

- metadata:

  a csvw metadata list

- location:

  the location of the metadata

## Value

metadata coerced into a [table group
description](https://www.w3.org/TR/tabular-metadata/#dfn-table-group-description)
