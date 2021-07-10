# CSV on the Web R Package (csvwr) <img src="man/figures/logo.png" align="right" height="139" />

[![build](https://github.com/Robsteranium/csvwr/actions/workflows/r.yml/badge.svg)](https://github.com/Robsteranium/csvwr/actions/workflows/r.yml)
[![pkgdown](https://github.com/Robsteranium/csvwr/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/Robsteranium/csvwr/actions/workflows/pkgdown.yml)

Read and write csv tables annotated with metadata according to the "CSV on the Web" standard (CSVW).

The [csvw model for tabular data](https://w3c.github.io/csvw/syntax/) describes how to annotate a group of csv tables to ensure they are interpreted correctly.

This package uses the [csvw metadata schema](https://w3c.github.io/csvw/metadata/) to find tables, identify column names and cast values to the correct types.

The aim is to reduce the amount of manual work needed to parse and prepare data before it can be used in analysis.


## Usage

### Reading CSVW

You can use `csvwr` to read a csv table with json annotations into a data frame:

```r
library(csvwr)

# Parse a csv table using json metadata :
csvw <- read_csvw("data.csv", "metadata.json")

# To extract the parsed table (with syntactic variable names and typed-columns):
csvw$tables[[1]]$dataframe
```

Alternatively, you can jump straight to the parsed table in one call:

```
read_csvw_dataframe("data.csv", "metadata.json")
```

### Writing CSVW

You can also prepare annotations for a data frame:

```r
# Given a data frame (saved as a csv)
d <- data.frame(x=c("a","b","c"), y=1:3)
write.csv(d, "table.csv", row.names=FALSE)

# Derive a schema
s <- derive_table_schema(d)

# Create metadata (as a list)
m <- create_metadata(tables=list(list(url="table.csv", tableSchema=s)))

# Serialise the metadata to JSON
j <- jsonlite::toJSON(m)

# Write the json to a file
cat(j, file="metadata.json")
```

For a complete introduction to the library please see the `vignette("read-write-csvw")`.


## Installation

You'll need to use devtools to install this package from github:

```r
install.packages("devtools")
devtools::install_github("Robsteranium/csvwr")
```

## Contributing

### Roadmap

Broadly speaking, the objectives are as follows:

- parse csvw, creating dataframes with specified names and types (mostly implemented)
- connecting associated csv tables and json files according to the conventions set out in the csvw standard (partly implemented)
- support for validating a table according to a metadata document (a little implemented)
- support for multiple tables (mostly implemented)
- tools for writing csvw metadata, given an R data frame (partly implemented)
- vignettes and documentation (mostly implemented)
- scripts for running the most useful tools from the command line (not yet implemented)

It's not an urgent objective for the library to perform csv2rdf or csv2json translation although some support for csv2json is provided as this is used to test that the parsing is done correctly.

In terms of the csvw test cases provided by the standard, the following areas need to be addressed (in rough priority order):

- datatypes (most of simple datatypes and some complex ones are supported, but there are more types and constraints too)
- validations (there are a lot of these 😊)
- propagation of inherited properties
- http retrieval (`readr::read_csv` (and indeed `utils::read.csv`) accepts URIs, but the spec also involves link, dialect, and content-type headers)
- referential integrity (a foundation for this is in place)
- json nesting

### Testing

The project currently incorporates two main parts of the [csvw test](https://w3c.github.io/csvw/tests/) suite:

- [Parsing with JSON output](https://github.com/Robsteranium/csvwr/blob/master/tests/testthat/test-csvw-parsing-json.R)
- [Validation](https://github.com/Robsteranium/csvwr/blob/master/tests/testthat/test-csvw-validation.R)

In each case, we're running only that subset of test entries that can be expected to pass given that part of the standard that has thus far been implemented. Some entries will be skipped (either permanently or) while other priorities are implemented.

You can find out what needs to be implemented next by widening the subset to include the next entry.

During development, you may find it convenient to recreate one of the test entries for exploration. There is a convenience function in [tests/csvw-tests-helpers.R](https://github.com/Robsteranium/csvwr/blob/master/tests/csvw-tests-helpers.R). This isn't exported by the package so you'll need to evaluate it explicitly. You can then use it as follows:

```r
run_entry_in_dev(16) # index number in the list of entries
run_entry_in_dev(id="manifest-json#test023") # identifier for the test
```

There are also some more [in-depth unit tests](https://github.com/Robsteranium/csvwr/blob/master/tests/testthat/test-parsing.R) written for this library.

### Workflow

You can use `devtools::load_all()` (`CTRL + SHIFT + L` in RStudio) to load updates and `testthat::test_local()` (`CTRL + SHIFT + T`) to run the tests.

In order to check the vignettes, you need to do `devtools::install(build_vignettes=T)`. Then you can open e.g. `vignette("read-write-csvw")`.

## License

GPL-3

To discuss other licensing terms, please [get in contact](mailto:csvw@infonomics.ltd.uk).

## Other CSVW tools

There's another R implementation of csvw in the package [rcsvw](https://github.com/davideceolin/rcsvw).

If you're interested in csvw more generally, then the [RDF::Tabular](https://github.com/ruby-rdf/rdf-tabular/) ruby gem provides one of the more robust and comprehensive implementations, supporting both translation and validation.

If you're specifically interested in validation, take a look at the [ODI](https://theodi.org/)'s [csvlint](https://github.com/Data-Liberation-Front/csvlint.rb) which implements csvw and also the [OKFN](https://okfn.org/)'s [frictionless data table schemas](https://specs.frictionlessdata.io/).

If you want rdf translation, then you might like to check out [Swirrl](https://www.swirrl.com/)'s [csv2rdf](https://github.com/Swirrl/csv2rdf/) and also [table2qb](https://github.com/swirrl/table2qb) which generates csvw annotations from csv files to describe [RDF Data Cubes](https://www.w3.org/TR/vocab-data-cube/).
