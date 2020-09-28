# https://w3c.github.io/csvw/tests/

run_entry <- function(entry) {
  csvw <- read_csvw(entry$action, metadata=entry$option$metadata)
  actual <- csvw_to_list(csvw)
  list(csvw=csvw, actual=actual)
}

name_test <- function(entry) {
  test_name <- paste0(entry$id, " - ", entry$name, "; action: ", entry$action)
  if(!is.null(entry$option$metadata)) {
    test_name <- paste0(test_name, ", metadata: ", entry$option$metadata)
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

# convenience function for loading-up an entry to explore a failing test
run_entry_in_dev <- function(n=NULL, id=NULL) {
  orig_op <- options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/",
                     csvwr_compatibility_mode=T)
  on.exit(options(orig_op), add=T, after=F)

  csvw_test_path <- "tests/csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  entries <- c("manifest-json.jsonld","manifest-validation.jsonld") %>%
    purrr::map(function(f) { jsonlite::read_json(f)$entries }) %>%
    do.call(what="c")

  if(!is.null(id)) {
    n <- which(sapply(entries, function(x) x$id == id))
  }
  entry <- entries[[n]]

  print(entry$name)
  print(entry$comment)

  run_entry(entry)
}
