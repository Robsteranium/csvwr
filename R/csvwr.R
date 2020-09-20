#' csvwr: Read and write CSV on the Web (CSVW)
#'
#' Read and write csv tables annotated with
#' \href{https://w3c.github.io/csvw/metadata/}{csvw metadata}. This helps to
#' ensure consistent processing and reduce the amount of manual work needed to
#' parse and prepare data before it can be used in analysis.
#'
#' @section Getting started:
#'
#' The best place to start is the
#' \href{../doc/read-write-csvw.html}{Reading and Writing CSVW} vignette.
#'
#' @section Reading annotated tables:
#' \itemize{
#'   \item \code{\link{read_csvw}} Parse a table group
#'   \item \code{\link{read_csvw_dataframe}} Parse a table group and extract the first
#'     data frame
#' }
#'
#' @section Writing table annotations:
#' \itemize{
#'   \item \code{\link{derive_table_schema}} Derive table schema from a data frame
#'   \item \code{\link{create_metadata}} Create a table group annotation
#'   \item \code{\link{derive_metadata}} Derive an annotation from a csv file
#' }
#'
#' @docType package
#' @name csvwr
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
NULL # roxygen needs to document something! https://r-pkgs.org/man.html#man-packages

# https://github.com/tidyverse/magrittr/issues/29
# Otherwise R Check complains:
# > no visible binding for global variable ‘.’
globalVariables(".")

#' Retreive the base URI from configuration
#'
#' @return returns the value of `csvwr_base_uri` option, defaulting to `example.net`
#' @examples
#' \dontrun{
#' base_uri() # returns default
#'
#' options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/")
#' base_uri()
#' }
base_uri <- function() {
  getOption("csvwr_base_uri", "http://example.net/")
}

#' Transform date/time format string from
#'
#' As per the [csvw specification for date and time
#' formats](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/#h-formats-for-dates-and-times)
#' we accept format strings using the [date field symbols defined in unicode
#' TR35](www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table).
#' These are converted to POSIX 1003.1 date format strings for use in
#' [base::strptime()] or [readr::parse_date()]/[readr::parse_datetime()].
#'
#' @param format_string a UAX35 date format string
#' @return a POSIX date format string
#' @examples
#' fmt <- transform_datetime_format("dd.MM.yyyy")
#' strptime("01.01.2001", format=fmt)
#' @md
transform_datetime_format <- function(format_string) {
  format_string %>%
    gsub("yyyy", "%Y", .) %>%
    gsub("dd", "%d", .) %>%
    gsub("MM", "%m", .) %>%
    gsub("(?<!%)d", "%d", ., perl=T) %>%
    gsub("(?<!%)M", "%m", ., perl=T) %>%
    gsub("HH", "%H", .) %>%
    gsub("mm", "%M", .)
}


#' Map csvw datatypes to R types
#'
#' Translate [csvw datatypes](https://www.w3.org/TR/tabular-metadata/#datatypes) to R types.
#' This implementation currently targets [readr::cols] column specifications.
#'
#' @param datatypes a list of csvw datatypes
#' @return a `readr::cols` specification - a list of collectors
#' @examples
#' cspec <- datatype_to_type(list("double", list(base="date", format="yyyy-MM-dd")))
#' readr::read_csv(readr::readr_example("challenge.csv"), col_types=cspec)
#' @md
datatype_to_type <- function(datatypes) {
  datatypes %>% purrr::map(function(datatype) {
    if(is.list(datatype)) {
      # complex types (list)
      switch(datatype$base,
             date = readr::col_date(format=transform_datetime_format(datatype$format)),
             datetime = readr::col_datetime(format=transform_datetime_format(datatype$format)),
             stop("unrecognised complex datatype: ", datatype))
    } else {
      # simple types (string)
      switch(datatype,
             integer = readr::col_integer(),
             double = readr::col_double(),
             number = readr::col_double(),
             string = readr::col_character(),
             date = readr::col_date(),
             gYear = readr::col_character(), # TODO xsd datatypes
             anyURI = readr::col_character(),
             stop("unrecognised simple datatype: ", datatype))
    }
  })
}

#' Read CSV on the Web
#'
#' If the argument to `filename` is a json metadata document, this will be used to find csv files for
#' each table using the value of `csvw:url`.
#'
#' If the argument to `filename` is a csv file, and no `metadata` is provided, an attempt is made to
#' derive metadata.
#'
#' If the argument to `filename` is a csv file, and the `metadata` is provided, then the given csv will
#' override the value of `csvw:url`.
#'
#' The csvw metadata is returned as a list. In each table in the table group, an element named
#' `dataframe` is added which provides the contents of the csv table parsed into a data frame using
#' the table schema.
#'
#' @param filename a path for a csv table or a json metadata document
#' @param metadata optional user metadata
#' @return csvw metdata list, with a `dataframe` property added to each table
#' @examples
#' \dontrun{
#' read_csvw("metadata.json")
#' read_csvw("table.csv", "metadata.json")
#' }
read_csvw <- function(filename, metadata=NULL) {
  metadata <- locate_metadata(filename, metadata)

  metadata$tables <- lapply(metadata$tables,
                            try_add_dataframe,
                            filename=filename,
                            dialect=metadata$dialect,
                            group_schema=metadata$tableSchema)

  metadata <- normalise_metadata(metadata, location=filename)

  return(metadata)
}

#' Add data frame to csvw table annotation
#'
#' @param table a `csvw:Table` annotation
#' @param filename a filename/ URL for the csv table
#' @param group_schema a fallback tableSchema from the table group
#' @return a table annotation with a `dataframe` attribute added with data frame
#' holding the contents of the table
#' @md
add_dataframe <- function(table, filename, dialect, group_schema) {
  schema <- table$tableSchema %||% group_schema
  if(is.null(schema)) {
    # if we need to derive a default schema, then set this on the table itself
    table$tableSchema <- schema <- default_schema(filename, dialect)
  }
  dialect <- dialect %||% default_dialect
  csv_url <- locate_table(filename, table$url)

  table_columns <- schema$columns[!coalesce_truth(schema$columns$virtual), ]
  column_names <- table_columns$name
  column_types <- datatype_to_type(table_columns$datatype)

  dtf <- readr::read_csv(csv_url,
                         trim_ws=T,
                         skip=dialect$headerRowCount,
                         col_names=column_names,
                         col_types=column_types)
  table$dataframe <- dtf

  table
}

#' Try to add a dataframe to the table
#'
#' If this fails, a list describing the error is added instead
try_add_dataframe <- function(table, ...) {
  tryCatch(add_dataframe(table, ...),
           error=function(e) {
             table$dataframe <- list("error"=e, ...)
             table
            })
}

#' Read a data frame from the first table in a csvw
#'
#' Wrapper around `read_csvw` convenient when you're only interested in the data and there's only one table
#' @param filename a path for a csv table or a json metadata document
#' @param metadata optional user metadata
#' @return a data frame parsed using the table schema
read_csvw_dataframe <- function(filename, metadata=NULL) {
  read_csvw(filename, metadata)$tables[[1]]$dataframe
}

#' Locate metadata for a table
#'
#' Follows the procedure defined in the [csvw model](https://w3c.github.io/csvw/syntax/#locating-metadata):
#'
#' 1. Metadata supplied by the user
#' 1. Metadata referenced by a link header
#' 1. Metadata located through default paths
#' 1. Metadata embedded in the file
#'
#' We extend this to use the [derive_metadata] function to inspect the table itself.
#'
#' @param filename a path for a csv table or a json metadata document
#' @param metadata optional user metadata
#' @return csvw metdata list
#' @md
locate_metadata <- function(filename, metadata) {
  if(!is.null(metadata)) {
    # overriding metadata
    metadata <- read_metadata(metadata)
  } else if(stringr::str_ends(filename,"\\.json")) {
    # file itself is the metadata
    metadata <- read_metadata(filename)
  } else {
    # retrieve file references by link header (TODO)

    # attempt to locate via defaults/ configuration
    location <- find_metadata(filename)
    if(!is.null(location)) {
      metadata <- read_metadata(location)
    } else {
      # use embedded metadata from the tabular file (TODO)

      # derive metadata from the table itself
      metadata <- derive_metadata(filename)
    }
  }
  return(metadata)
}

#' Locate csv data table
#'
#' @param filename the file passed to `read_csvw` in the first place (could be the csv or json annotations)
#' @param url the location of the the table as defined in the metadata
locate_table <- function(filename, url) {
  # attempt to locate locally
  local_filename <- paste(dirname(filename), url, sep="/")
  # if argument to read_csvw was a csv use that, otherwise use the table's `csvw:url`
  location <- ifelse(stringr::str_ends(filename, "\\.csv"), filename, url)

  find_existing_file(local_filename) %||% location
} # TODO: unit test me!

#' Set the base of a URI template
#'
#' @param t a character vector of URI templates
#' @param url a filename url being used as a context (string)
#' @return a character vector of templates with base paths/ domains set appropriately
set_uri_base <- function(t, url) {
  # prefix current domain if rendered template starts with a `/`
  if(strtrim(t,1)=="/") {
    domain <- paste(strsplit(url, "/")[[1]][c(1,3)], collapse="//")
    t <- paste0(domain, t)
  } else {
    # prefix current path if template doesn't start with `{+url}`
    if(strtrim(t,6)!="{+url}") {
      path <- dirname(url)
      t <- paste(path, t, sep="/")
    }
  }
  t
}

#' Render URI templates
#'
#' This doesn't yet implement the whole of RFC 6570, just enough to make the tests pass
#'
#' You can bind variables by passing a list to the explicit `bindings` argument,
#' or variadically with `...` by naming arguments according to the variable name you wish to bind.
#'
#' @param templates a character vector with URI templates
#' @param bindings a list of variable bindings to be interpolated into templates
#' @param ... further bindings specified as named function arguments
#' @examples
#' render_uri_templates("{+url}/resource?query=value", list(url="http://example.net"))
#' render_uri_templates("{+url}", url="http://example.net")
#' @importFrom string str_replace
#' @importFrom string str_glue_data
#' @md
render_uri_templates <- function(templates, bindings=NULL, ...) {
  bindings <- c(bindings, list(...))
  templates %>%
    purrr::map(set_uri_base, url=bindings$url) %>%
    stringr::str_replace("\\+", "") %>%
    purrr::map_chr(function(t) stringr::str_glue_data(bindings, t))
}

#' Identify metadata location configurations for a tabular file
#'
#' Returns default locations. Will ultimately retrieve remote configuration
#'
#' @param filename a csv file
#' @return a character vector of URI templates
location_configuration <- function(filename) {
  default_locations <- c("{+url}-metadata.json","csv-metadata.json")
  # TODO retrieve site-wide configuration from `/.well-known/csvm`
  # https://w3c.github.io/csvw/syntax/#default-locations-and-site-wide-location-configuration
  default_locations
}


find_existing_file <- function(filenames) {
  for(candidate in filenames) {
    if(file.exists(candidate)) {
      found <- candidate
      break
    }
  }

  if(exists("found")) {
    return(found)
  } else {
    return(NULL)
  }
}


#' Find metadata for a tabular file
#'
#' Searches through the default locations attempting to locate metadata.
#'
#' @param filename a csv file
#' @return a uri for the metadata, or null if none were found
find_metadata <- function(filename) {
  candidates <- render_uri_templates(location_configuration(filename), url=filename)

  find_existing_file(candidates)
}

#' Read and parse CSVW Metadata
#'
#' Reads in a json document as a list, transforming columns specifications into a dataframe.
#'
#' @param filename a path for a json metadata document
#' @return csvw metdata list
read_metadata <- function(filename) {
  metadata <- jsonlite::read_json(filename)
  metadata <- parse_metadata(metadata, location=filename)
  metadata$tables <- lapply(metadata$tables, function(t) {
    if(!is.null(t$tableSchema)) {
      t$tableSchema$columns <- parse_columns(t$tableSchema$columns)
    }
    t
  })
  if(!is.null(metadata$tableSchema)) {
    metadata$tableSchema$columns <- parse_columns(metadata$tableSchema$columns)
  }
  metadata
}

#' Identify the type of an annotation property
#'
#' This can be done from the name of the property. The
#' [metadata descriptor diagram](https://w3c.github.io/csvw/metadata/properties.svg)
#' provides a succinct summary of the types of each property and how they fit together
#'
#'  @param property a named list element
#'  @return a string defining the property type
#'  @examples
#'  property_type(list(url="http://example.net"))
#'  @md
property_type <- function(property) {
  stopifnot(length(property)==1)
  if(is.null(names(property))) {
    return("atomic") # can't determine type of unnamed property, must be atomic
  }
  switch(names(property),
         "tables"="array",
         "transformations"="array",
         "notes"="array",
         "@context"="array",
         "foreignKeys"="array",
         "columns"="array",
         "lineTerminators"="array",
         "url"="link",
         "@id"="link",
         "resource"="link",
         "schemaReference"="link",
         "targetFormat"="link",
         "scriptFormat"="link",
         "aboutUrl"="uri_template",
         "propertyUrl"="uri_template",
         "valueUrl"="uri_template",
         "primaryKey"="column_reference",
         "rowTitles"="column_reference",
         "columnReference"="column_reference",
         "titles"="natural_language",
         "tableSchema"="object",
         "dialect"="object",
         "reference"="object",
         "atomic")
}

base_url <- function(metadata, location) {
  context <- metadata$`@context`
  base <- if(is.list(context)) {
    context[2]$`@base`
  }
  if(is.null(base)) {
    base <- base_uri()
  }
  url <- resolve_url(base, location)
  url
}

resolve_url <- function(url1, url2) {
  sep <- ifelse(stringr::str_ends(url1,"/"),"","/")
  combined <- paste(url1, url2, sep=sep)
  combined %>% stringr::str_replace("/./","/")
}

normalise_url <- function(property, base_url) {
  # do nothing if the property already includes a URL scheme
  if(grepl("://", property)) {
    property
  } else {
    resolve_url(base_url, property)
  }
}

#' Normalise an annotation property
#'
#' This follows the [normalisation](https://w3c.github.io/csvw/metadata/#normalization)
#' process set out in the csvw specification.
#' @param property an annotation property (a list)
#' @return a property (list) a
normalise_property <- function(property, base_url) {
  normalised_property <- switch(property_type(property),
    link = normalise_url(property, base_url),
    property)
  if(is.list(normalised_property)) {
    normalised_property
  } else {
    # ensure we return a list named as per the input
    stats::setNames(list(normalised_property), nm=names(property))
  }
}

#' Parse metadata
#'
#' Coerces the metadata to ensure it describes a table group.
#' Retreives any linked tableSchema.
#'
#' @param metadata a csvw metadata list
#' @param location the location of the metadata
#' @return metadata coerced into a
#'   [table group description](https://www.w3.org/TR/tabular-metadata/#dfn-table-group-description)
#' @md
parse_metadata <- function(metadata, location) {
  # coerce to table group description
  if(is.null(metadata$tables)) {
    if(is.null(metadata$url)) {
      stop("Metadata doesn't define any tables, or a url to build a table definition from")
      # Could still be valid with a top-level schema, dialect or transformation
      # https://w3c.github.io/csvw/metadata/#top-level-properties
    } else {
      # put context at top level and everything else into a table group description
      metadata <- list(`@context`=metadata$`@context`,
                       tables=list(metadata[names(metadata) != "@context"]))
    }
  }

  # retrieve linked tableSchema
  metadata$tables <- lapply(metadata$tables, function(table) {
    # read list in place if is a link (not a list itself)
    if(class(table$tableSchema)=="character") {
      table$tableSchema <- jsonlite::read_json(file.path(dirname(location), table$tableSchema))
    }
    table
  })

  metadata
}

#' Normalise metadata
#'
#' The spec defines a [normalisation process](https://w3c.github.io/csvw/metadata/#normalization).
#'
#' @param metadata a csvw metadata list
#' @param location the location of the metadata
#' @return metadata with normalised properties
#' @md
normalise_metadata <- function(metadata, location) {
  # normalise annotations
  metadata <- rlmap(metadata, normalise_property, base_url=base_url(metadata, dirname(location)))

  metadata
}

#' Parse columns schema
#'
#' @param columns a list of lists specification of columns
#' @return a data frame with a row per column specification
parse_columns <- function(columns) {
  d <- list_of_lists_to_df(columns)

  # required defaults to false
  if(is.null(d$required)) {
    d$required <- F
  } else {
    d$required <- sapply(d$required, function(x) ifelse(is.null(x) || is.na(x),F,x))
  }

  if(is.null(d$datatype)) {
    d$datatype <- "string"
  } else {
    d$datatype <- lapply(d$datatype, function(x) if(is.null(x) || is.na(x)) { "string" } else { x })
  }

  d
}

#' Coalesce value to truthiness
#'
#' Determineness whether the input is true, with missing values being interpreted as false.
#'
#' @param x logical, `NA` or `NULL`
#' @return `FALSE` if x is anything but `TRUE`
coalesce_truth <- function(x) {
  if(is.null(x)) {
    F
  } else {
    ifelse(is.na(x), F, x)
  }
}

#' Parse list of lists specification into a data frame
#'
#' @param ll a list of lists
#' @return a data frame with a row per list
list_of_lists_to_df <- function(ll) {
  # need to get all names, not just use those from first column
  nms <- ll %>% purrr::map(names) %>% purrr::reduce(union)

  purrr::transpose(ll, .names=nms) %>%
    # coalesce missing specifications to NA
    purrr::map(function(l) { lapply(l, function(x) { if(is.null(x)) { NA } else { x }}) }) %>%
    purrr::simplify_all() %>%
    # prevents lists being split into columns (allows cells to be lists)
    purrr::map_if(is.list, I) %>%
    as.data.frame(stringsAsFactors=F, col.names=nms)
}




#' Derive csvw metadata from a csv file
#'
#' @param filename a csv file
#' @return a list of csvw metadata
#' @examples
#' \dontrun{
#' derive_metadata("example.csv")
#' }
derive_metadata <- function(filename) {
  schema <- default_schema(filename, list(header=T))
  create_metadata(tables=list(
    list(url = paste0(base_uri(), filename), tableSchema=schema)
  ))
}

#' Derive csvw table schema from a data frame
#'
#' @param d a data frame
#' @return a list describing a `csvw:tableSchema`
#' @examples
#' \dontrun{
#' derive_table_schema(data.frame(a=1,b=2))
#' }
derive_table_schema <- function(d) {
  # TODO: derive snake-case names, datatypes from column types
  cols <- colnames(d)
  list(
    columns=data.frame(name=cols, titles=cols, datatype="string", stringsAsFactors = F)
  )
}


#' Create a default table schema given a csv file and dialect
#'
#' If neither the table nor the group have a `tableSchema` annotation,
#' then this default schema will used.
#'
#' @param filename a csv file
#' @param dialect specification of the csv's dialect
#' @return a table schema
#' @md
default_schema <- function(filename, dialect) {
  data_sample <- readr::read_csv(filename, n_max=10, col_names=dialect$header, col_types=readr::cols())
  if(!dialect$header) {
    names(data_sample) <- paste0("_col.", 1:ncol(data_sample))
  }
  derive_table_schema(data_sample)
}

#' CSVW default dialect
#'
#' The [CSVW Default Dialect specification](https://w3c.github.io/csvw/metadata/#dialect-descriptions)
#' described in [CSV Dialect Description Format](http://dataprotocols.org/csv-dialect/).
#'
#' @return a list specifying a default csv dialect
default_dialect <- list(
  encoding="utf-8",
  lineTerminators=c("\r\n","\n"),
  quoteChar="\"",
  doubleQuote=T,
  skipRows=0,
  commentPrefix="#",
  header=T,
  headerRowCount=1,
  delimiter=",",
  skipColumns=0,
  skipBlankRows=F,
  skipInitialSpace=F,
  trim=F
)

#' Create tabular metadata from a list of tables
#'
#' The table annotations should each be a list with keys for `url` and `tableSchema`.
#' You can use `derive_table_schema` to derive a schema from a data frame.
#'
#' @param tables a list of `csvw:table` annotations
#' @return a list describing a tabular metadata annotation
create_metadata <- function(tables) {
  list("@context"="http://www.w3.org/ns/csvw", tables = tables)
}


#' Check for blank values
#'
#' Is the value NA or an empty string `""`?
#'
#' @param value any vector
#' @return a logical indicating if the value is blank
#' @examples
#' \dontrun{
#' is_blank(c(NA, "", 1, TRUE, "a"))
#' }
is_blank <- function(value) {
  is.na(value) | (value=="")
}

is_non_core_annotation <- function(property) {
  !any(names(property) %in% c("tables","tableScheme","url","@context","columns"))
}

#' Convert json-ld annotation to json
#'
#' Follows the [rules for JSON-LD to JSON conversion set out in the csv2json
#' standard](https://w3c.github.io/csvw/csv2json/#json-ld-to-json).
#' @md
json_ld_to_json <- function(property) {
  stopifnot(length(property)==1)
  value <- property[[1]]
  if(is.list(value)) {
    value <- compact_json_ld(value)
  }
  stats::setNames(list(value), nm=names(property))
}

#' Compact objects to values
#'
#' Follows the [rules for JSON-LD to JSON conversion set out in the csv2json
#' standard](https://w3c.github.io/csvw/csv2json/#json-ld-to-json).
#' @md
compact_json_ld <- function(value) {
  if(is.list(value)) {
    if(any(c("@id","@value") %in% names(value))) {
      value <- value[[1]]
    } else {
      lapply(value, compact_json_ld)
    }
  } else {
    value
  }
}


#' Serialise cell values for JSON representation
#'
#' @param cell a typed value
#' @return a representation comparable with the JSON representation (typically a string)
render_cell <- function(cell) {
  switch(class(cell)[1],
         Date = strftime(cell),
         POSIXct = strftime(cell),
         POSIXlt = strftime(cell),
         cell)
}

#' @importFrom magrittr %>%
table_to_list <- function(table, group_schema, dialect) {
  table <- purrr::lmap_if(table, is_non_core_annotation, json_ld_to_json, .else=identity)
  schema <- table$tableSchema %||% group_schema # TODO: check that normalize doesn't obviate this
  header_row_count <- (dialect %||% default_dialect)$headerRowCount
  row_num <- 0
  table$row <- purrr::pmap(table$dataframe, list) %>%
    purrr::map(function(r) {
      row_num <<- row_num + 1
      url <- paste0(table$url, "#row=", row_num + header_row_count)
      names(r) <- schema$columns[!suppressWarnings(coalesce_truth(schema$columns$virtual)), ]$name
      r <- lapply(r, render_cell)
      if(!is.null(schema$aboutUrl)) {
        template <- paste0("{+url}",schema$aboutUrl)
        r <- c(list("@id"=render_uri_templates(template,bindings=c(url=table$url,r))),r)
      }
      row_description <- list(purrr::discard(r, .p=is_blank))
      list(url=url, rownum=row_num, describes=row_description)
    })
  table[!(names(table) %in% c("tableSchema","dataframe"))]
}

#' Convert a csvw metadata to a list (csv2json)
#'
#' @param csvw a csvw metadata list
#' @return a list following the csv2json translation rules
#' @examples
#' \dontrun{
#' csvw_to_list(read_csvw("example.csv")))
#' }
csvw_to_list <- function(csvw) {
  list(tables=lapply(csvw$tables, table_to_list, group_schema=csvw$tableSchema, dialect=csvw$dialect))
}

#csvw_triples <- function(csvw) # returns vector of triples/ s-p-o data.frame

#' Get path to csvwr example
#'
#' The csvwr package includes some example csvw files in it's `inst/extdata` directory.
#' You can use this function to find them.
#'
#' Inspired by [readr::readr_example()]
#'
#' @param path The filename. If `NULL`, the example files will be listed.
#' @return either a file path or a vector of filenames
#' @examples
#' csvwr_example()
#' csvwr_example("computer-scientists.csv")
#' @md
csvwr_example <- function(path=NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "csvwr"))
  }
  else {
    system.file("extdata", path, package = "csvwr", mustWork = TRUE)
  }
}
