#' Validate CSVW specification
#'
#' Follows the [csvw table validation](https://w3c.github.io/csvw/syntax/#validating-tables)
#' procedure.
#'
#' @param csvw a csvw metadata specification (a list)
#' @return a validation report (list)
#' @md
validate_csvw <- function(csvw) {
  # TODO: primary key check, table/tablegroup/ schema compatibility, cell errors
  # https://w3c.github.io/csvw/metadata/#dfn-validator
  report <- list(references=validate_referential_integrity(csvw))
  report <- purrr::compact(report)
  report$is_valid <- length(report)==0 #all(sapply(report, is.null))
  report
}

validate_referential_integrity <- function(csvw) {
  table_checks <- lapply(csvw$tables, function(table) {
    fk_checks <- lapply(table$tableSchema$foreignKeys, function(fk) {
      key <- table$dataframe %>% purrr::pluck(fk$columnReference) %>% unique()
      foreign_table <- extract_table(csvw, fk$reference)
      foreignKey <- foreign_table$dataframe %>% purrr::pluck(fk$reference$columnReference) %>% unique()
      #missing_key <- foreignKey[!(foreignKey %in% key)]
      missing_foreignKey <- key[!(key %in% foreignKey)]
      if(length(missing_foreignKey)>0) {
        list(
          constraint=fk,
          # missingKey=missing_key,
          missing_foreignKey=missing_foreignKey)
      }
    })
    fk_checks <- purrr::compact(fk_checks)
    if(length(fk_checks)>0) {
      list(url=table$url, constraints=fk_checks)
    }
  })
  purrr::compact(table_checks)
}

extract_table <- function(csvw, reference) {
  if(!is.null(reference$resource) & !is.null(reference$schemaReference)) {
    stop("The foreignKey reference must not have both a schemaReference and a resource")
  }

  if(!is.null(reference$resource)) {
    criteria <- quote(table$url == reference$resource)
  } else if(!is.null(reference$schemaReference)) {
    criteria <- quote(table$tableSchema$`@id` == reference$schemaReference)
  } else {
    stop("The foreignKey reference must have either a schemaReference or a resource")
  }

  matches <- which(sapply(csvw$tables, function(table) { eval(criteria) }))

  if(length(matches)==0) {
    stop("The referenced foreign table or schema was not found with: ", format(criteria))
  } else if(length(matches)==1) {
    csvw$tables[[matches]]
  } else {
    stop("More than one table or schema matches the foreign key reference")
  }
}
