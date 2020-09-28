source("../csvw-tests-helpers.R")

test_parsing <- function(entry) {
  test_that(name_test(entry), {
    test_result <- run_entry(entry)
    expected <- jsonlite::read_json(entry$result)
    expect_equal(test_result$actual, expected)
  })
}

describe("CSVW Parsing Tests (JSON)", {
  # https://w3c.github.io/csvw/tests/

  orig_op <- options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/",
                     csvwr_compatibility_mode=T)
  on.exit(options(orig_op), add=T, after=F)

  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  test_manifest <- jsonlite::read_json(file.path(csvw_test_path, "manifest-json.jsonld"))
  skipped_entries <- c(11,13) # link header
  entries <- test_manifest$entries[setdiff(1:16, skipped_entries)]

  lapply(entries, test_parsing)
})

# need to use implicit to set-up a mock directory, perhaps
# `with_mock(list.files=file_that_can_only_find_implicits)`
# https://www.rdocumentation.org/packages/testthat/versions/2.3.2/topics/with_mock

#actual <- csvw_to_list(read_csvw(file.path(csvw_test_path, "test005.csv")))
#expected <- jsonlite::read_json(file.path(csvw_test_path, "test005.json"))
#all.equal(actual$tables[[1]]$row[[1]], expected$tables[[1]]$row[[1]])
