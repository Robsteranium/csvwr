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
#' @keywords internal
"_PACKAGE" # roxygen needs to document something! https://r-pkgs.org/man.html#man-packages

# https://github.com/tidyverse/magrittr/issues/29
# Otherwise R Check complains:
# > no visible binding for global variable ‘.’
globalVariables(".")

#' Retrieve the base URI from configuration
#'
#' @return returns the value of `csvwr_base_uri` option, defaulting to `example.net`
#' @examples
#' \dontrun{
#' base_uri() # returns default
#'
#' options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/")
#' base_uri()
#' }
#' @export
base_uri <- function() {
  getOption("csvwr_base_uri", "http://example.net/")
}

#' Transform date/time format string from Unicode TR35 to POSIX 1003.1
#'
#' As per the [csvw specification for date and time
#' formats](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/#h-formats-for-dates-and-times)
#' we accept format strings using the [date field symbols defined in unicode
#' TR35](https://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table).
#' These are converted to POSIX 1003.1 date format strings for use in
#' [base::strptime()] or [readr::parse_date()]/[readr::parse_datetime()].
#'
#' @param format_string a UAX35 date format string
#' @return a POSIX date format string
#' @examples
#' \dontrun{
#' fmt <- transform_datetime_format("dd.MM.yyyy")
#' strptime("01.01.2001", format=fmt)
#' }
#' @md
#' @export
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
#' \dontrun{
#' cspec <- datatype_to_type(list("double", list(base="date", format="yyyy-MM-dd")))
#' readr::read_csv(readr::readr_example("challenge.csv"), col_types=cspec)
#' }
#' @md
#' @export
datatype_to_type <- function(datatypes) {
  datatypes %>% purrr::map(function(datatype) {
    if(is.list(datatype)) {
      # complex types (specified with a list)
      switch(datatype$base %||% "string",
             boolean = readr::col_character(), # TODO: parse as logical
             date = readr::col_date(format=transform_datetime_format(datatype$format)),
             datetime = readr::col_datetime(format=transform_datetime_format(datatype$format)),
             decimal = readr::col_double(), # TODO: validate max/min
             string = readr::col_character(), # TODO: use format regex in validation
             stop("unrecognised complex datatype: ", datatype))
    } else {
      # simple types (specified with a string)
      switch(datatype %||% "string",
             integer = readr::col_integer(), # TODO: more variants
             double = readr::col_double(),
             float = readr::col_double(),
             number = readr::col_double(),
             decimal = readr::col_double(),
             string = readr::col_character(), # TODO: more variants
             boolean = readr::col_logical(),
             date = readr::col_date(),
             datetime = readr::col_datetime(),
             time = readr::col_time(),
             duration = readr::col_character(),
             gDay = readr::col_character(), # TODO: xsd datatypes
             gMonth = readr::col_character(),
             gMonthDay = readr::col_character(),
             gYear = readr::col_character(),
             gYearMonth = readr::col_character(),
             xml = readr::col_character(),
             html = readr::col_character(),
             json = readr::col_character(),
             binary = readr::col_character(), # Base 64
             hexBinary = readr::col_character(),
             QName = readr::col_character(),
             anyURI = readr::col_character(),
             any = readr::col_character(),
             normalizedString = readr::col_character(),
             stop("unrecognised simple datatype: ", datatype))
    }
    # TODO: value and length constraints
  })
}


#' Map R types to csvw datatype
#'
#' Translate R types to [csvw datatypes](https://www.w3.org/TR/tabular-metadata/#datatypes).
#' Acts as an inverse of `datatype_to_type` but doesn't provide a 1:1 correspondence.
#'
#' @param types a list of R types
#' @return a list of csvw datatypes
#' @md
#' @export
type_to_datatype <- function(types) {
  types %>% purrr::map(function(type) {
    switch(type[[1]],
           numeric = "number",
           integer = "integer",
           double = "double",
           character = "string",
           factor = "string",
           logical = "boolean",
           Date = "date",
           "string")
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
#' @return csvw metadata list, with a `dataframe` property added to each table
#' @examples
#' \dontrun{
#' read_csvw("metadata.json")
#' read_csvw("table.csv", "metadata.json")
#' }
#' @export
#' @md
read_csvw <- function(filename, metadata=NULL) {
  metadata <- locate_metadata(filename, metadata)

  metadata$tables <- lapply(metadata$tables,
                            try_add_dataframe,
                            filename=filename,
                            group=metadata)

  metadata <- normalise_metadata(metadata, location=filename)

  return(metadata)
}

#' Add data frame to csvw table annotation
#'
#' @param table a `csvw:Table` annotation
#' @param filename a filename/ URL for the csv table
#' @param group a list of metadata for the table group to use as a fallback
#' @return a table annotation with a `dataframe` attribute added with data frame
#' holding the contents of the table
#' @md
#' @keywords internal
add_dataframe <- function(table, filename, group) {
  schema <- table$tableSchema %||% group$tableSchema
  dialect <- override_defaults(table$dialect, group$dialect, default_dialect)
  if(is.null(schema)) {
    # if we need to derive a default schema, then set this on the table itself
    table$tableSchema <- schema <- default_schema(filename, dialect)
  }
  csv_url <- locate_table(filename, table$url)

  table_columns <- schema$columns[!coalesce_truth(schema$columns[["virtual"]]), ]
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
#'
#' @param table a `csvw:Table` annotation
#' @param ... arguments to `add_dataframe`
#' @return A table annotation with a `dataframe` attribute added with data frame
#' holding the contents of the table or an error.
#' @md
#' @keywords internal
try_add_dataframe <- function(table, ...) {
  tryCatch(add_dataframe(table, ...),
           error=function(e) {
             cli::cli_alert_danger("Could not read data frame from {.path {table$url}} because of {e$message}")
             table$dataframe <- list(error=e,  ...)
             table
            })
}

#' Read a data frame from the first table in a csvw
#'
#' Wrapper around `read_csvw` convenient when you're only interested in the data and there's only one table
#' @param filename a path for a csv table or a json metadata document
#' @param metadata optional user metadata
#' @return A data frame parsed using the table schema
#' @export
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
#' @return csvw metadata list
#' @md
#' @keywords internal
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
#' @return The location of the table
#' @keywords internal
locate_table <- function(filename, url) {
  # append table's url to location of filename
  url_relative_to_filename <- paste(dirname(filename), url, sep="/")

  # if argument to read_csvw was a csv use that, otherwise use the table's `csvw:url`
  ifelse(stringr::str_ends(filename, "\\.csv"), filename, url_relative_to_filename)
} # TODO: unit test me!

#' Set the base of a URI template
#'
#' @param t a character vector of URI templates
#' @param url a filename url being used as a context (string)
#' @return a character vector of templates with base paths/ domains set appropriately
#' @keywords internal
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
#' Interpolate variable bindings into a URI template.
#'
#' This doesn't yet implement the whole of RFC 6570, just enough to make the tests pass
#'
#' You can bind variables by passing a list to the explicit `bindings` argument,
#' or variadically with `...` by naming arguments according to the variable name you wish to bind.
#'
#' @param templates a character vector with URI templates
#' @param bindings a list of variable bindings to be interpolated into templates
#' @param ... further bindings specified as named function arguments
#' @return a character vector with the expanded URI
#' @examples
#' render_uri_templates("{+url}/resource?query=value", list(url="http://example.net"))
#' render_uri_templates("{+url}", url="http://example.net")
#' @importFrom stringr str_replace
#' @importFrom stringr str_glue_data
#' @export
#' @md
render_uri_templates <- function(templates, bindings=NULL, ...) {
  bindings <- c(bindings, list(...))
  templates %>%
    purrr::map(set_uri_base, url=bindings$url) %>%
    stringr::str_replace("\\+", "") %>%
    # https://datatracker.ietf.org/doc/html/rfc6570#section-3.2.4
    stringr::str_replace("\\{#", "#{") %>%
    purrr::map_chr(function(t) stringr::str_glue_data(bindings, t))
}

#' Identify metadata location configurations for a tabular file
#'
#' Returns default locations. Will ultimately retrieve remote configuration
#'
#' @param filename a csv file
#' @return a character vector of URI templates
#' @keywords internal
location_configuration <- function(filename) {
  default_locations <- c("{+url}-metadata.json","csv-metadata.json")
  # TODO retrieve site-wide configuration from `/.well-known/csvm`
  # https://w3c.github.io/csvw/syntax/#default-locations-and-site-wide-location-configuration
  default_locations
}

#' Find the first existing file from a set of candidates
#'
#' @param filenames a vector of candidates
#' @return
#' If one of the filenames passed is found, then the first is returned.
#' If none of the filenames exist, `NULL` is returned
#' @keywords internal
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
#' @keywords internal
find_metadata <- function(filename) {
  candidates <- render_uri_templates(location_configuration(filename), url=filename)

  find_existing_file(candidates)
}

#' Read and parse CSVW Metadata
#'
#' Reads in a json document as a list, transforming columns specifications into a dataframe.
#'
#' @param filename a path for a json metadata document
#' @return csvw metadata list
#' @export
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
#' provides a succinct summary of the types of each property and how they fit together.
#'
#' @param property a named list element
#' @return a string defining the property type
#' @examples
#' property_type(list(url="http://example.net"))
#' @md
#' @noRd
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

#' Determine the base URL for CSVW metadata
#'
#' @param metadata the csvw metadata
#' @param location where the metadata was originally located
#' @return A string containing the base URL
#' @keywords internal
base_url <- function(metadata, location) {
  context <- metadata$`@context`
  base <- if(is.list(context)) {
    context[2]$`@base`
  }
  if(is.null(base)) {
    base <- base_uri()
  }
  url <- normalise_url(location, base)
  url
}

#' Resolve one URL against another
#'
#' @param url1 the base url
#' @param url2 a relative url
#' @return A single absolute url
#' @keywords internal
resolve_url <- function(url1, url2) {
  sep <- ifelse(stringr::str_ends(url1,"/"),"","/")
  combined <- paste(url1, url2, sep=sep)
  combined %>% stringr::str_replace("/./","/")
}

#' Does the string provide an absolute URL
#'
#' @param string the url, path or template
#' @return true if the string is an absolute url
#' @keywords internal
is_absolute_url <- function(string) {
  #grepl("://", string) # contains a URL scheme
  grepl(":", string) # works with cURIes
}

#' Normalise a URL
#'
#' Ensures that a url is specified absolutely with reference to a base
#'
#' @param url a string
#' @param base the base to use for normalisation
#' @return A string containing a normalised URL
#' @keywords internal
normalise_url <- function(url, base) {
  # do nothing if the url is already absolute
  if(is_absolute_url(url)) {
    url
  } else {
    resolve_url(base, url)
  }
}

#' Normalise an annotation property
#'
#' This follows the [normalisation](https://w3c.github.io/csvw/metadata/#normalization)
#' process set out in the csvw specification.
#' @param property an annotation property (a list)
#' @param base_url the base URL for normalisation
#' @return a property (list) a
#' @keywords internal
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
#' Retrieves any linked tableSchema.
#'
#' @param metadata a csvw metadata list
#' @param location the location of the metadata
#' @return metadata coerced into a
#'   [table group description](https://www.w3.org/TR/tabular-metadata/#dfn-table-group-description)
#' @md
#' @keywords internal
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
    if(isa(table$tableSchema, "character")) {
      table$tableSchema <- jsonlite::read_json(normalise_url(table$tableSchema,dirname(location)))
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
#' @keywords internal
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
    d$datatype <- lapply(d$datatype, function(x) if(any(is.null(x) | is.na(x))) { "string" } else { x })
  }

  d
}

#' Coalesce value to truthiness
#'
#' Determine whether the input is true, with missing values being interpreted as false.
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

#' Unlist unless the list-elements are themselves lists
#'
#' Convert a list of elements to a vector. Unlike `base::unlist` this doesn't
#' convert the elements of inner lists to vector elements. Thus only a list a single
#' layer deep is flattened to a vector.
#'
#' @param l a list
#' @return A list of lists or a vector
#' @keywords internal
unlist1 <- function(l) {
  is_list <- vapply(l, is.list, logical(1))
  if(!any(is_list)) {
    unlist(l)
  } else {
    l
  }
}

#' Parse list of lists specification into a data frame
#'
#' @param ll a list of lists
#' @return a data frame with a row per list
#' @keywords internal
list_of_lists_to_df <- function(ll) {
  # need to get all names, not just use those from first list per transpose
  nms <- ll %>% purrr::map(names) %>% purrr::reduce(union)

  purrr::transpose(ll, .names=nms) %>%
    # coalesce missing specifications to NA
    purrr::map(function(l) { lapply(l, function(x) { if(is.null(x)) { NA } else { x }}) }) %>%
    # convert single-depth lists into vectors
    purrr::map(unlist1) %>%
    # prevents lists being split into columns (allows cells to be lists)
    purrr::map_if(is.list, I) %>%
    as.data.frame(stringsAsFactors=F, col.names=nms)
}




#' Derive csvw metadata from a csv file
#'
#' @param filename a csv file
#' @return a list of csvw metadata
#' @examples
#' derive_metadata(csvwr_example("computer-scientists.csv"))
#' @export
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
#' derive_table_schema(data.frame(a=1,b=2))
#' @export
derive_table_schema <- function(d) {
  cols <- colnames(d) # TODO: derive snake-case names instead of make.names dots

  types <- if(coalesce_truth(getOption("csvwr_compatibility_mode"))) {
    "string"
  } else {
    type_to_datatype(unname(lapply(d,class))) %>% purrr::simplify()
  }

  columns <- data.frame(name=cols, titles=cols, stringsAsFactors = F)
  columns$datatype <- types # list in constructor would've led to a variable per list element

  list(columns=columns)
}


#' Create a default table schema given a csv file and dialect
#'
#' If neither the table nor the group have a `tableSchema` annotation,
#' then this default schema will used.
#'
#' @param filename a csv file
#' @param dialect specification of the csv's dialect (default: `default_dialect`)
#' @return a table schema
#' @md
default_schema <- function(filename, dialect=default_dialect) {
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

#' Override defaults
#'
#' Merges two lists applying `override` values on top of the `default` values.
#'
#' @param ... any number of lists with configuration values
#'
#' @return a list with the values from the first list replacing those in the second and so on
#' @keywords internal
override_defaults <- function(...) {
  dialect <- list()

  set_value <- function(x) { dialect[names(x)] <<- x }

  purrr::walk(rev(list(...)), function(l) {
    purrr::lmap(l, set_value)
  })

  dialect
}

#' Create tabular metadata from a list of tables
#'
#' The table annotations should each be a list with keys for `url` and `tableSchema`.
#' You can use `derive_table_schema` to derive a schema from a data frame.
#'
#' @param tables a list of `csvw:table` annotations
#' @return a list describing a tabular metadata annotation
#' @examples
#' d <- data.frame(foo="bar")
#' table <- list(url="filename.csv", tableSchema=derive_table_schema(d))
#' create_metadata(tables=list(table))
#' @export
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
#' @noRd
is_blank <- function(value) {
  is.na(value) | (value=="")
}

#' Determine if an annotation is non-core
#'
#' Checks if the annotation is non-core, and should thus be treated as a json-ld note.
#'
#' @param property a list element
#' @return `TRUE` the annotation is core, `FALSE` otherwise
#' @keywords internal
is_non_core_annotation <- function(property) {
  !any(names(property) %in% c("tables","tableScheme","url","@context","columns"))
}

#' Convert json-ld annotation to json
#'
#' Follows the [rules for JSON-LD to JSON conversion set out in the csv2json
#' standard](https://w3c.github.io/csvw/csv2json/#json-ld-to-json).
#'
#' @param property a json-ld annotation (single list element)
#' @return A compacted list element
#' @md
#' @keywords internal
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
#'
#' @param value an element from a list (could be a vector or another list)
#' @return A compacted value.
#' @keywords internal
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
#' @keywords internal
render_cell <- function(cell) {
  switch(class(cell)[1],
         Date = strftime(cell),
         POSIXct = strftime(cell),
         POSIXlt = strftime(cell),
         cell)
}

#' Convert a table to a list
#'
#' Follows the pattern for csv2json
#' @param table the csvw table
#' @param group list of metadata for the group used for a fallback schema and dialect
#' @return a list representation of the table's contents
#' @importFrom magrittr %>%
#' @keywords internal
table_to_list <- function(table, group) {
  table <- purrr::lmap_if(table, is_non_core_annotation, json_ld_to_json, .else=identity)
  schema <- table$tableSchema %||% group$tableSchema # TODO: check that normalize doesn't obviate this
  dialect <- override_defaults(table$dialect, group$dialect, default_dialect)
  row_num <- 0
  table$row <- purrr::pmap(table$dataframe, list) %>%
    purrr::map(function(r) {
      row_num <<- row_num + 1
      url <- paste0(table$url, "#row=", row_num + dialect$headerRowCount)
      suppressWarnings(
        names(r) <- schema$columns[!suppressWarnings(coalesce_truth(schema$columns$virtual)), ]$name
      )
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
#' csvw_to_list(read_csvw("example.csv"))
#' }
#' @export
csvw_to_list <- function(csvw) {
  list(tables=lapply(csvw$tables, table_to_list, group=csvw))
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
#' @export
#' @md
csvwr_example <- function(path=NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "csvwr"))
  }
  else {
    system.file("extdata", path, package = "csvwr", mustWork = TRUE)
  }
}
