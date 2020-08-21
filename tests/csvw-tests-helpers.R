# https://w3c.github.io/csvw/tests/

run_entry <- function(entry) {
  csvw <- read_csvw(entry$action, metadata=entry$options$metadata)
  actual <- csvw_to_list(csvw)
  list(csvw=csvw, actual=actual)
}

name_test <- function(entry) {
  test_name <- paste0(entry$id, " - ", entry$name, " action: ", entry$action)
  if(!is.null(entry$options$metadata)) {
    test_name <- paste0(test_name, ", metadata: ", entry$options$metadata)
  }
  if(!is.null(entry$result)) {
    test_name <- paste0(test_name, ", result: ", entry$result)
  }
  test_name
}

find_entry <- function(id) {
  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  test_manifest <- jsonlite::read_json(file.path(csvw_test_path, "manifest-json.jsonld"))
  entry <- test_manifest$entries[[which(sapply(test_manifest$entries, function(x) x$id == id))]]
  entry
}
