# CSV on the Web R Package (csvwr)

Read and write CSV on the web.

The csvw tabular metadata standard describes a group of csv tables and can be used to ensure they are interpreted properly.

This package uses the metadata schema to find tables, identify column names and cast values to the correct types.

## Roadmap

- basic csvw parsing, with names and types
- associating csv tables and json files according to the conventions set out in the csvw standard
- support for validating a table according to a metadata document
- support for multiple tables
- vignettes and documentation
