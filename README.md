# CSV on the Web R Package (csvwr)

Read and write CSV on the Web.

The [csvw model for tabular data](https://w3c.github.io/csvw/syntax/) standard describes how to annotate a group of csv tables to ensure they are interpreted correctly.

This package uses the [csvw metadata schema](https://w3c.github.io/csvw/metadata) to find tables, identify column names and cast values to the correct types.

The aim is to reduce the amount of manual work needed to parse and prepare data before it can be used in analysis.


## Usage

You can use `csvwr` to read a csv table with json annotations into a data frame:

```r
library(csvwr)

# To load a csvw metadata and parse the table:
csvw <- read_csvw("data.csv", "metadata.json")

# To extract the parsed table (with syntactic variable names and typed-columns):
csvw$tables[[1]]$dataframe

# To load the csvw metadata and extract the table in one call:
read_csvw_dataframe("data.csv", "metadata.json")
```

You can also prepare annotations for a data frame:

```r
# Given a data frame
d <- data.frame(x=c("a","b","c"), y=1:3)

# Derive a schema
s <- derive_table_schema(d)

# Create metadata (as a list)
m <- create_metadata(tables=s)

# Serialise the metadata to JSON
j <- jsonlite::toJSON(m)

# Write the json to a file
cat(j, file="metadata.json")
```

For a complete introduction to the library please see the vignette [Read and Write CSVW in R](vignettes/read-write-csvw.html).


## Installation

You'll need to use devtools to install this package from github:

```r
install.packages("devtools")
devtools::install_github("Robsteranium/csvwr")
```


## Development

### Roadmap

Broadly speaking, the objectives are as follows:

- basic csvw parsing, with names and types (partly implemented)
- associating csv tables and json files according to the conventions set out in the csvw standard (partly implemented)
- support for validating a table according to a metadata document (not implemented)
- support for multiple tables (not implemented)
- tools for writing csvw metadata, given an R data frame (partly implemented)
- vignettes and documentation (partly implemented)
- scripts for running the most useful tools from the command line (not implemented)

It's not an urgent objective for the library to perform csv2rdf or csv2json translation although some support for csv2json is provided as this is used to test the parsing is done correctly.

In terms of the csvw test cases provided by the standard, the following areas need to be address (in rough priority order):

- datatypes
- validations (many)
- propagation of inherited properties
- http retrieval (link, dialect, content-type headers etc)
- referential integrity
- json nesting

### Testing

The project currently incorporates two main parts of the [csvw test](https://w3c.github.io/csvw/tests/) suite:

- [Parsing with JSON output](tests/testthat/test-csvw-parsing-json.R)
- [Validation](tests/testthat/test-csvw-validation.R)

In each case, we're running only that subset of test entries that can be expected to pass given that part of the standard that has thus far been implemented. Some entries will be skipped (either permanently or) while other prioritites are implemented.

You can find out what needs to be implemented next by widening the subset to include the next entry.

During development, you may find it convenient to recreate a one of the test entries for exploration. There is a convenience function in `tests/csvw-tests-helpers.R`. This isn't exported by the package so you'll need to evaluate it explicitly. You can then use it as follows:

```r
run_entry_in_dev(16) # index number in the list of entries
run_entry_in_dev(id="manifest-json#test023") # identifier for the test
```

There are also some more [in-depth unit tests](tests/testthat/test-parsing.R) written for this library.

### Workflow

You can use `devtools::load_all()` (`CTRL + SHIFT + L` in RStudio) to load updates and `testthat::test_local()` (`CTRL + SHIFT + T`) to run the tests.

In order to check the vignettes, you need to do `devtools::install(build_vignettes=T)`. Then you can open e.g. `vignette("read-write-csvw")`.


### License

GPL-3

To discuss other licensing terms, please get in contact.
