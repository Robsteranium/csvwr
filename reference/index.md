# Package index

## Reading annotated tables

- [`read_csvw()`](https://robsteranium.github.io/csvwr/reference/read_csvw.md)
  : Read CSV on the Web
- [`read_csvw_dataframe()`](https://robsteranium.github.io/csvwr/reference/read_csvw_dataframe.md)
  : Read a data frame from the first table in a csvw
- [`csvwr_example()`](https://robsteranium.github.io/csvwr/reference/csvwr_example.md)
  : Get path to csvwr example
- [`extract_table()`](https://robsteranium.github.io/csvwr/reference/extract_table.md)
  : Extract a referenced table from CSVW metadata
- [`read_metadata()`](https://robsteranium.github.io/csvwr/reference/read_metadata.md)
  : Read and parse CSVW Metadata

## Writing table annotations

- [`derive_table_schema()`](https://robsteranium.github.io/csvwr/reference/derive_table_schema.md)
  : Derive csvw table schema from a data frame
- [`create_metadata()`](https://robsteranium.github.io/csvwr/reference/create_metadata.md)
  : Create tabular metadata from a list of tables
- [`derive_metadata()`](https://robsteranium.github.io/csvwr/reference/derive_metadata.md)
  : Derive csvw metadata from a csv file
- [`default_dialect`](https://robsteranium.github.io/csvwr/reference/default_dialect.md)
  : CSVW default dialect
- [`default_schema()`](https://robsteranium.github.io/csvwr/reference/default_schema.md)
  : Create a default table schema given a csv file and dialect

## Validating table annotations

- [`normalise_metadata()`](https://robsteranium.github.io/csvwr/reference/normalise_metadata.md)
  : Normalise metadata
- [`validate_csvw()`](https://robsteranium.github.io/csvwr/reference/validate_csvw.md)
  : Validate CSVW specification
- [`validate_referential_integrity()`](https://robsteranium.github.io/csvwr/reference/validate_referential_integrity.md)
  : Validate the referential integrity of a csvw table group

## Utility functions

- [`base_uri()`](https://robsteranium.github.io/csvwr/reference/base_uri.md)
  : Retrieve the base URI from configuration
- [`render_uri_templates()`](https://robsteranium.github.io/csvwr/reference/render_uri_templates.md)
  : Render URI templates
- [`csvw_to_list()`](https://robsteranium.github.io/csvwr/reference/csvw_to_list.md)
  : Convert a csvw metadata to a list (csv2json)
- [`type_to_datatype()`](https://robsteranium.github.io/csvwr/reference/type_to_datatype.md)
  : Map R types to csvw datatype
- [`datatype_to_type()`](https://robsteranium.github.io/csvwr/reference/datatype_to_type.md)
  : Map csvw datatypes to R types
- [`transform_datetime_format()`](https://robsteranium.github.io/csvwr/reference/transform_datetime_format.md)
  : Transform date/time format string from Unicode TR35 to POSIX 1003.1
- [`coalesce_truth()`](https://robsteranium.github.io/csvwr/reference/coalesce_truth.md)
  : Coalesce value to truthiness
