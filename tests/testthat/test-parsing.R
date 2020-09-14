csvw_test_path <- "../csvw-tests"

test_that("Basic parsing works", {
  result <- read_csvw_dataframe(file.path(csvw_test_path, "test001.csv"))

  expect_equal(nrow(result), 8)
  expect_equal(colnames(result), c("Surname","FamilyName"))
  expect_equal(result[5, ]$Surname, "Maggie")
})

compsci_csv <- system.file("extdata", "computer-scientists.csv", package = "csvwr", mustWork = TRUE)
compsci_json <- system.file("extdata", "computer-scientists.json", package = "csvwr", mustWork = TRUE)

test_that("Target features", {
  result <- read_csvw_dataframe(compsci_csv, metadata=compsci_json)

  expect_s3_class(result, "data.frame")

  test_that("Variables names taken from metadata", {
    expect_equal(colnames(result), c("name","dob"))
  })

  test_that("Datatypes taken from metadata", {
    types <- lapply(result, class)
    expect_equal(types$name, "character")
    expect_equal(types$dob, "Date")
  })
})

test_that("Metadata can be derived from a csv", {
  orig_dir <- setwd(system.file("extdata", ".", package = "csvwr", mustWork = TRUE))
  on.exit(setwd(orig_dir), add=T, after=F)

  metadata <- derive_metadata("computer-scientists.csv")
  expected <- list(
    "@context"="http://www.w3.org/ns/csvw",
    tables=list(
      list(
        url="http://example.net/computer-scientists.csv",
        tableSchema=list(
          columns=data.frame(name=c("Name","Date Of Birth"),
                             titles=c("Name","Date Of Birth"),
                             datatype="string",
                             stringsAsFactors = F)
        )
      )
    )
  )
  expect_equal(metadata, expected)
})

test_that("Metadata may be found from a filename", {
  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  metadata <- find_metadata("test011/tree-ops.csv")
  expect_equal(metadata, "test011/tree-ops.csv-metadata.json")
})

test_that("URI Templates may be rendered", {
  # https://w3c.github.io/csvw/syntax/#default-locations-and-site-wide-location-configuration
  expect_equal(render_uri_templates(c("{+url}-metadata.json", "csv-metadata.json"),
                                    url="http://example.org/south-west/devon.csv"),
               c("http://example.org/south-west/devon.csv-metadata.json",
                 "http://example.org/south-west/csv-metadata.json"))

  expect_equal(render_uri_templates(c("{+url}.json", "csvm.json", "/csvm?file={url}.json"), # last is modified from spec
                                    url="http://example.org/south-west/devon.csv"),
               c("http://example.org/south-west/devon.csv.json",
                 "http://example.org/south-west/csvm.json",
                 "http://example.org/csvm?file=http://example.org/south-west/devon.csv.json"))

  # filesystem tests
  expect_equal(render_uri_templates(c("{+url}-metadata.json", "csv-metadata.json"),
                                    url=file.path(csvw_test_path, "test011/tree-ops.csv")),
               c("../csvw-tests/test011/tree-ops.csv-metadata.json",
                 "../csvw-tests/test011/csv-metadata.json"))

  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)
  expect_equal(render_uri_templates(c("{+url}-metadata.json", "csv-metadata.json"),
                                    url="test011/tree-ops.csv"),
               c("test011/tree-ops.csv-metadata.json",
                 "test011/csv-metadata.json"))
})

test_that("Date Formats may be transformed", {
  expect_equal(transform_datetime_format("M/d/yyyy"), "%m/%d/%Y")
  examples <- list(
    c(fmt="yyyy-MM-dd",  eg="2015-03-22"),
    c(fmt="yyyyMMdd",  eg="20150322"),
    c(fmt="dd-MM-yyyy",  eg="22-03-2015"),
    c(fmt="d-M-yyyy",  eg="22-3-2015"),
    c(fmt="MM-dd-yyyy",  eg="03-22-2015"),
    c(fmt="M-d-yyyy",  eg="3-22-2015"),
    c(fmt="dd/MM/yyyy",  eg="22/03/2015"),
    c(fmt="d/M/yyyy",  eg="22/3/2015"),
    c(fmt="MM/dd/yyyy",  eg="03/22/2015"),
    c(fmt="M/d/yyyy",  eg="3/22/2015"),
    c(fmt="dd.MM.yyyy",  eg="22.03.2015"),
    c(fmt="d.M.yyyy",  eg="22.3.2015"),
    c(fmt="MM.dd.yyyy",  eg="03.22.2015"),
    c(fmt="M.d.yyyy",  eg="3.22.2015"))
  lapply(examples, function(e) {
    fmt <- transform_datetime_format(e["fmt"])
    expect_equal(unname(strptime(!!e["eg"], format=!!fmt)), as.POSIXlt("2015-03-22"))
  })
})


test_that("coalesce", {
  expect_true(coalesce_truth(T))
  expect_false(coalesce_truth(NULL))
  expect_false(coalesce_truth(F))

  expect_equal(coalesce_truth(c(T,F,NA)), c(T,F,F))
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
