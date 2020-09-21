source("../csvw-tests-helpers.R")

test_validation <- function(entry) {
  test_that(name_test(entry), {
    test_result <- run_entry(entry)
    expected <- switch (entry$type,
                        "csvt:PositiveValidationTest" = T,
                        "csvt:NegativeValidationTest" = F,
                        stop("Unexpected test type: ", entry$id))
    result <- validate_csvw(test_result$csvw)



    # show validation report in tests when fails unexpectedly
    if(expected & !result$is_valid) {
      expect_equal(result,
                   list(references=list(), is_valid=TRUE))
    } else {
      expect_equal(result$is_valid, expected)
    }
  })
}

describe("CSVW Validation Tests", {
  orig_op <- options(csvwr_base_uri="http://www.w3.org/2013/csvw/tests/")
  on.exit(options(orig_op), add=T, after=F)

  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  test_manifest <- jsonlite::read_json(file.path(csvw_test_path, "manifest-validation.jsonld"))
  entries <- test_manifest$entries[1:29]

  lapply(entries, test_validation)
})



# need to use implicit to set-up a mock directory, perhaps `with_mock(list.files=file_that_can_only_find_implicits)`
# https://www.rdocumentation.org/packages/testthat/versions/2.3.2/topics/with_mock
