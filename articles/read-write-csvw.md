# Read and Write CSVW in R

## What is CSV on the Web?

The W3C’s CSV on the Web Working Group produced a series of
recommendations for working with tabular data on the web.

- The [Model for Tabular Data and Metadata on the
  Web](https://w3c.github.io/csvw/syntax/) describes how to annotate
  tables with metadata, and how processing applications should locate
  metadata and parse annotated tables.
- The [Metadata Vocabulary for Tabular
  Data](https://w3c.github.io/csvw/metadata/) provides a specification
  for annotating tables with metadata at various levels, from groups of
  tables and how they relate to each other down to individual cells
  within a table.
- This vocabulary provides a mapping so that annotated tables can be
  transform from CSV to other formats:
  [csv2json](https://w3c.github.io/csvw/csv2json/) and
  [csv2rdf](https://w3c.github.io/csvw/csv2rdf/)

The `csvwr` library implements parts of this standard in R. The overall
goal of the project is to support reading and writing of annotated CSV
tables, in order to ensure consistent processing and reduce the amount
of manual work needed to parse and prepare data before it can be used in
analysis.

Practically speaking, you annotate a csv file by providing an
accompanying json document containing the metadata. We benefit from
annotating tables with csvw benefit because:

- the csv dialect (i.e. the choice of field separator or quoting
  characters) is explicitly defined, helping to avoid parsing mistakes
- columns can be given syntactically-valid variable names (while
  retaining human-readable labels for display)
- cell types are declared, obviating the need to e.g. parse dates (and
  figure-out formatting strings)

This package includes some example csv and json for you to explore. Here
is the csv file:

``` r

library(csvwr)

compsci_csv <- csvwr_example("computer-scientists.csv")

cat(readLines(compsci_csv),sep="\n")
#> Name,Date Of Birth
#> Barbara Liskov,1939-11-07
#> Evelyn Boyd Granville,1924-05-01
#> Ada Lovelace,1815-12-10
```

Here is the annotation:

``` r

compsci_json <- csvwr_example("computer-scientists.json")

cat(readLines(compsci_json),sep="\n")
#> {
#>   "@context": "http://www.w3.org/ns/csvw",
#>   "tables": [{
#>     "url": "computer-scientists.csv",
#>     "tableSchema": {
#>       "columns": [{
#>         "name": "name",
#>         "titles": "Name",
#>         "datatype": "string",
#>         "propertyUrl": "foaf:name"
#>       }, {
#>         "name": "dob",
#>         "titles": "Date Of Birth",
#>         "datatype": "date",
#>         "propertyUrl": "schema:birthDate"
#>       }],
#>       "aboutUrl": "http://example.org/computer-scientsts/{#name}",
#>       "primaryKey": "name"
#>     }
#>   }]
#> }
```

## How do I read a table annotated with CSVW?

You can get up and running quickly using the
[read_csvw_dataframe](https://robsteranium.github.io/csvwr/help/read_csvw_dataframe)
function:

``` r

d <- read_csvw_dataframe(compsci_csv, compsci_json)
```

This parses the csvw metadata, and uses that to parse and interpret the
csv file. A data frame is returned with columns that are named and typed
accordingly to the specifications in the table’s schema. Here you can
see the “Date Of Birth” field has been given a syntactically valid
variable name, and parsed into a date vector automatically:

``` r

str(d)
#> 'data.frame':    3 obs. of  2 variables:
#>  $ name: chr  "Barbara Liskov" "Evelyn Boyd Granville" "Ada Lovelace"
#>  $ dob : Date, format: "1939-11-07" "1924-05-01" ...

knitr::kable(d)
```

| name                  | dob        |
|:----------------------|:-----------|
| Barbara Liskov        | 1939-11-07 |
| Evelyn Boyd Granville | 1924-05-01 |
| Ada Lovelace          | 1815-12-10 |

This function assumes that you’re only interested in one table, and that
you don’t want to work with the csvw metadata itself. The metadata
vocabulary allows us to describe groups of tables (useful for lookup
tables). You can get the table and annotations using the
[read_csvw](https://robsteranium.github.io/csvwr/help/read_csvw)
function:

``` r

csvw <- read_csvw(compsci_csv, compsci_json)
```

This returns a nested list, which broadly follows the structure of the
csvw metadata.

As you can see from the json above, the `tables` element provides a list
of tables and the annotation for each table provides a `url` (this can
be used to locate csv files from the json metadata alone) and a
`tableSchema`.

Within the `tableSchema` we have annotations for each column. We parse
these into a data frame (instead of a list of lists which is how the
`jsonlite` library would ordinarily interpret a json array of objects).
This is much more idiomatic for manipulation in R.

``` r

csvw$tables[[1]]$tableSchema$columns
#>   name        titles datatype..string. datatype..date.      propertyUrl
#> 1 name          Name            string            date        foaf:name
#> 2  dob Date Of Birth            string            date schema:birthDate
#>   required
#> 1    FALSE
#> 2    FALSE
```

We also introduce another element to each table, named `dataframe`. This
provides the result of parsing the csv table using the schema provided
in the json:

``` r

csvw$tables[[1]]$dataframe
#>                    name        dob
#> 1        Barbara Liskov 1939-11-07
#> 2 Evelyn Boyd Granville 1924-05-01
#> 3          Ada Lovelace 1815-12-10
```

The function
[read_csvw_dataframe](https://robsteranium.github.io/csvwr/help/read_csvw_dataframe)
is just a convenience wrapper for calling
[read_csvw](https://robsteranium.github.io/csvwr/help/read_csvw) and
extracting this data frame.

## How do I create CSVW table annotations?

You can of course write json metadata by hand, but if you already have
your table as a data frame in R then we can use this to get a head
start.

If you provide the
[derive_table_schema](https://robsteranium.github.io/csvwr/help/derive_table_schema)
function with a data frame it will prepare some table annotations.

``` r

d <- data.frame(x=c("a","b","c"), y=1:3)
(s <- derive_table_schema(d))
#> $columns
#>   name titles datatype
#> 1    x      x   string
#> 2    y      y  integer
```

Notice that the column names, titles, and datatypes have been derived
from the data frame.

You can of course refine the schema further if you wish (e.g. to declare
[further constraints on the
datatypes](https://w3c.github.io/csvw/syntax/#datatypes) or to add [uri
templates](https://w3c.github.io/csvw/metadata/#uri-template-properties)).

You can then pass this to
[create_metadata](https://robsteranium.github.io/csvwr/help/create_metadata)
to build up a complete annotation.

First we build the table description. This requires that we provide a
URL for where the csv can be found.

For example, if we save the data frame to a local file:

``` r

write.csv(d, "table.csv", row.names=FALSE)
```

Then we can create a table description using the filename as the URL and
the schema we created earlier:

``` r

tb <- list(url="table.csv", tableSchema=s)
```

Relative URLs like this make sense when the json metadata and csv table
are to be found in the same place. You may instead want to unambiguously
locate the file with an absolute URL like
`https://raw.githubusercontent.com/Robsteranium/csvwr/master/inst/extdata/computer-scientists.csv`
which will work even if the metadata and table are held in different
locations.

Now we can build our complete annotation:

``` r

(m <- create_metadata(tables=list(tb)))
#> $`@context`
#> [1] "http://www.w3.org/ns/csvw"
#> 
#> $tables
#> $tables[[1]]
#> $tables[[1]]$url
#> [1] "table.csv"
#> 
#> $tables[[1]]$tableSchema
#> $tables[[1]]$tableSchema$columns
#>   name titles datatype
#> 1    x      x   string
#> 2    y      y  integer
```

This can then be serialised to JSON:

``` r

j <- jsonlite::toJSON(m)
jsonlite::prettify(j)
#> {
#>     "@context": [
#>         "http://www.w3.org/ns/csvw"
#>     ],
#>     "tables": [
#>         {
#>             "url": [
#>                 "table.csv"
#>             ],
#>             "tableSchema": {
#>                 "columns": [
#>                     {
#>                         "name": "x",
#>                         "titles": "x",
#>                         "datatype": "string"
#>                     },
#>                     {
#>                         "name": "y",
#>                         "titles": "y",
#>                         "datatype": "integer"
#>                     }
#>                 ]
#>             }
#>         }
#>     ]
#> }
#> 
```

This JSON may then be written to disk:

``` r

cat(j, file="metadata.json")
```
