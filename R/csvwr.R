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
#' @return a character vector of types
datatype_to_type <- function(x) {
  mapping  <- c("string" = "character",
                "date" = "Date")
  unname(mapping[x])
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
  if(is.null(metadata)) {
    metadata <- derive_metadata(filename)
  } else {
    metadata <- read_metadata(metadata)
  }

  schema <- metadata$tables[[1]]$tableSchema  # TODO: multiple tables
  column_names <- schema$columns$name
  column_types <- datatype_to_type(schema$columns$datatype)
  dtf <- utils::read.csv(filename, stringsAsFactors = F, strip.white=T, col.names=column_names, colClasses=column_types)
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
#' @importFrom magrittr %>%
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
  data_sample <- utils::read.csv(filename, nrows=1, check.names=F)
  list(
    "@context"="http://www.w3.org/ns/csvw",
    tables = list(
      list(
        url = paste0(base_uri(), filename),
        tableSchema=derive_table_schema(data_sample)
      )
    )
  )
}

#' Derive csvw table schema from a data frame
#'
#' @param d a data frame
#' @return csvw:tableSchema in a list
#' @examples
#' \dontrun{
#' derive_table_schema(data.frame(a=1,b=2))
#' }
derive_table_schema <- function(d) {
  cols <- colnames(d)
  list(
    columns=data.frame(name=make.names(cols), titles=cols, datatype="string")
  )
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
