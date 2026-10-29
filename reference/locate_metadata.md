# Locate metadata for a table

Follows the procedure defined in the [csvw
model](https://w3c.github.io/csvw/syntax/#locating-metadata):

## Usage

``` r
locate_metadata(filename, metadata)
```

## Arguments

- filename:

  a path for a csv table or a json metadata document

- metadata:

  optional user metadata

## Value

csvw metadata list

## Details

1.  Metadata supplied by the user

2.  Metadata referenced by a link header

3.  Metadata located through default paths

4.  Metadata embedded in the file

We extend this to use the
[derive_metadata](https://robsteranium.github.io/csvwr/reference/derive_metadata.md)
function to inspect the table itself.
