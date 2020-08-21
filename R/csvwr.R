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

#' Read CSV on the Web
#'
#' If the argument to `file` is a json metadata document, this will be used to find csv files for
#' each table using the value of `csvw:url`.
#'
#' If the argument to `file` is a csv file, and no metadata is provided, an attempt is made to
#' derive metadata.
#'
#' If the argument to `file` is a csv file, and the metadata is provided, then the given csv will
#' override the value of `csvw:url`.
#'
#' The csvw metadata is returned as a list. In each table in the table group, an element named
#' `dataframe` is added which provides the contents of the csv table parsed into a data frame using
#' the table schema.
#'
#' @param file a path for a csv table or a json metadata document
#' @param metadata optional user metadata
#' @return csvw metdata list, with a `dataframe` property added to each table
#' @examples
#' \dontrun{
#' read_csvw("metadata.json")
#' read_csvw("table.csv", "metadata.json")
#' }
read_csvw <- function(file, metadata=NULL) {
  if (!is.null(metadata)) {
    metadata <- jsonlite::read_json(metadata)
    schema <- metadata$tables[[1]]$tableSchema  # TODO: multiple tables
    column_names <- sapply(schema$columns, function(x) x[["name"]])
    column_types <- sapply(schema$columns, function(x) {
      switch(x[["datatype"]],
             "string" = "character",
             "date" = "Date")
    })
    url <- metadata$url
    df <- utils::read.csv(file, stringsAsFactors = F, strip.white=T, col.names= column_names, colClasses=column_types)
  } else {
    url <- paste0(base_uri(), file)
    df <- utils::read.csv(file, stringsAsFactors = F, strip.white=T, colClasses="character")
  }
  # TODO: multiple tables
  csvw <- list(tables=list(list(url=url, dataframe=df)))
  return(csvw)
}

first_dataframe <- function(csvw) {
  csvw$tables[[1]]$dataframe
}

validate_csvw <- function(csvw) {
  T
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
  # rows <- apply(table$dataframe, 1, function(row) {
  #   row_num <<- row_num + 1
  #   url <- paste0(table$url, "#row=", row_num+1)  # ought to check for header, this is row num on original table
  #   list(url=url, rownum=row_num, describes=list(as.list(row)))
  # })
  rows <- purrr::transpose(table$dataframe) %>%
    purrr::map(function(r) {
      row_num <<- row_num + 1
      url <- paste0(table$url, "#row=", row_num+1)  # ought to check for header, this is row num on original table
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
#derive_metadata <- function(filename) # returns metadata list
