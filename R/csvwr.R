#' csvwr: Read and write CSV on the Web (CSVW)
#'
#' Read and write csv tables annotated with
#' \href{https://w3c.github.io/csvw/metadata/}{csvw metadata}. This helps to
#' ensure consistent processing and reduce the amount of manual work needed to
#' parse and prepare data before it can be used in analysis.
#'
#' @section Getting started:
#'
#' The best place to start is the \link[=../doc/read-write-csvw.html]{Reading and writing CSVW vignette}.
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
NULL # roxygen needs to document something! https://r-pkgs.org/man.html#man-packages

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

#' Map csvw datatypes to R types
#'
#' @param x a character vector of datatypes
#' @return compact string representation for `readr::cols` with one character per column-type
datatype_to_type <- function(x) {
  mapping  <- c("string" = "c",
                "date" = "D")
  paste0(mapping[x],collapse="")
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

  schema <- metadata$tables[[1]]$tableSchema  # TODO: multiple tables
  column_names <- schema$columns$name
  column_types <- datatype_to_type(schema$columns$datatype)
  dtf <- readr::read_csv(filename,
                         trim_ws=T,
                         skip=1,
                         col_names=column_names,
                         col_types=column_types)
  metadata$tables[[1]]$dataframe <- dtf

  return(metadata)
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
  } else {
    # retreive file references by link header (TODO)

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

#' Set the base of a URI template
#'
#' @param t a URI template (character vector)
#' @param url a filename url being used as a context (string)
#' @return a vector of templates with base paths/ domains set appropriately
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
#' @param templates a character vector with URI templates
#' @param ... bindings for variables to be interpolated into templates
render_uri_templates <- function(templates, ...) {
  bindings <- list(...)
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

#' Find metadata for a tabular file
#'
#' Searches through the default locations attempting to locate metadata.
#'
#' @param filename a csv file
#' @return a uri for the metadata, or null if none were found
find_metadata <- function(filename) {
  candidates <- render_uri_templates(location_configuration(filename), url=filename)

  for(candidate in candidates) {
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

#' Read and parse CSVW Metadata
#'
#' Reads in a json document as a list, transforming columns specifications into a dataframe.
#'
#' @param filename a path for a json metadata document
#' @return csvw metdata list
read_metadata <- function(filename) {
  metadata <- jsonlite::read_json(filename)
  metadata$tables <- lapply(metadata$tables, function(t) {
    t$tableSchema$columns <- list_of_lists_to_df(t$tableSchema$columns)
    t
  })
  metadata
}

#' Parse list of lists specification into a data frame
#'
#' @param ll a list of lists
#' @return a data frame with a row per list
list_of_lists_to_df <- function(ll) {
  nms <- ll %>% purrr::map(names) %>% purrr::reduce(union) # need to get all names, not just use those from first column
  purrr::transpose(ll, .names=nms) %>% purrr::simplify_all() %>% as.data.frame(stringsAsFactors=F)
}


validate_csvw <- function(csvw) {
  T
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
  data_sample <- readr::read_csv(filename, n_max=1, col_types=readr::cols())
  create_metadata(tables=list(
    list(
      url = paste0(base_uri(), filename),
      tableSchema=derive_table_schema(data_sample)
    )
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
  cols <- colnames(d)
  list(
    columns=data.frame(name=make.names(cols), titles=cols, datatype="string", stringsAsFactors = F)
  )
}

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

#' @importFrom magrittr %>%
table_to_list <- function(table) {
  row_num <- 0
  rows <- purrr::transpose(table$dataframe) %>%
    purrr::map(function(r) {
      row_num <<- row_num + 1
      url <- paste0(table$url, "#row=", row_num+1)  # ought to check for header, this is row num on original table
      names(r) <- table$tableSchema$columns$titles
      list(url=url, rownum=row_num, describes=list(purrr::discard(r, .p=is_blank)))
    })
  list(url=table$url, row=rows)
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
  list(tables=lapply(csvw$tables, table_to_list))
}

#csvw_triples <- function(csvw) # returns vector of triples/ s-p-o data.frame
