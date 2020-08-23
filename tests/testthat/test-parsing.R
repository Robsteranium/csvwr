csvw_test_path <- "../csvw-tests"

test_that("Basic parsing works", {
  result <- first_dataframe(read_csvw(file.path(csvw_test_path, "test001.csv")))

  expect_equal(nrow(result), 8)
  expect_equal(colnames(result), c("Surname","FamilyName"))
  expect_equal(result[5, "Surname"], "Maggie")
})

result <- first_dataframe(read_csvw("computer-scientists.csv", metadata="computer-scientists.json"))

test_that("Variables names taken from metadata", {
  expect_equal(colnames(result), c("name","dob"))
})

test_that("Datatypes taken from metadata", {
  types <- lapply(result, class)
  expect_equal(types$name, "character")
  expect_equal(types$dob, "Date")
})

test_that("Metadata can be derived from a csv", {
  metadata <- derive_metadata("computer-scientists.csv")
  expected <- list(
    "@context"="http://www.w3.org/ns/csvw",
    tables=list(
      list(
        url="http://example.net/computer-scientists.csv",
        tableSchema=list(
          columns=data.frame(name=c("Name","Date.Of.Birth"),
                             titles=c("Name","Date Of Birth"),
                             datatype="string")
        )
      )
    )
  )
  expect_equal(metadata, expected)
})

# test:
# - unit test type coercion
# - multiple tables
# should row.names(csvw$tables[[1]]) be set to match csvw (either original row num or output row num)
# read_csvw("metadata.json") # returns list of tables
# read_csvw("metadata.json", "table.csv") # override table's url (do we need a list to identify the table?)
# read_csvw("metadata.json")[[1]] # take first table
# read_csvw("metadata.json")[["foo"]] # take table called foo
#
# validate_csvw(csvw)
# validate_csvw("metadata.json")
# validate_csvw("table.csv")
#
# write_csvw("metadata.json")[["foo"]]
