csvw_test_path <- "../csvw-tests"

test_that("Basic parsing works", {
  result <- read_csvw_dataframe(file.path(csvw_test_path, "test001.csv"))

  expect_equal(nrow(result), 8)
  expect_equal(colnames(result), c("Surname","FamilyName"))
  expect_equal(result[5, ]$Surname, "Maggie")
})

test_that("Target features", {
  result <- read_csvw_dataframe(csvwr_example("computer-scientists.csv"),
                                metadata=csvwr_example("computer-scientists.json"))

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

test_that("Table schema may be derived", {
  d <- data.frame(char=c("a"),
                  logi=c(T),
                  num=c(1),
                  stringsAsFactors = F)

  result <- derive_table_schema(d)
  expected <- list(
    columns=data.frame(name=c("char","logi","num"),
                       titles=c("char","logi","num"),
                       datatype=c("string","boolean","number"),
                       stringsAsFactors = F))
  expect_equal(result, expected)
})

describe("Metadata can be derived from a csv", {
  orig_dir <- setwd(system.file("extdata", ".", package = "csvwr", mustWork = TRUE))
  on.exit(setwd(orig_dir), add=T, after=F)

  test_that("Table schema has string-types (as per spec)", {
    orig_op <- options(csvwr_compatibility_mode=T)
    on.exit(options(orig_op), add=T, after=F)

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

  test_that("Table schema has guessed-types", {
    metadata <- derive_metadata("computer-scientists.csv")
    expected <- list(
      "@context"="http://www.w3.org/ns/csvw",
      tables=list(
        list(
          url="http://example.net/computer-scientists.csv",
          tableSchema=list(
            columns=data.frame(name=c("Name","Date Of Birth"),
                               titles=c("Name","Date Of Birth"),
                               datatype=c("string","date"),
                               stringsAsFactors = F)
          )
        )
      )
    )
    expect_equal(metadata, expected)
  })
})

test_that("Metadata may be found from a filename", {
  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  metadata <- find_metadata("test011/tree-ops.csv")
  expect_equal(metadata, "test011/tree-ops.csv-metadata.json")
})

test_that("Tables may be located", {
  csvw_test_path <- "../csvw-tests"
  orig_dir <- setwd(csvw_test_path)
  on.exit(setwd(orig_dir), add=T, after=F)

  csvw <- read_csvw("test034/csv-metadata.json")
  lapply(csvw$tables, function(table) {
    if(inherits(table$dataframe,"data.frame")) {
      # attached successfully
      expect_true(inherits(table$dataframe, "data.frame"))
    } else {
      # failed (test report error)
      expect_null(table$dataframe$error$message)
    }
  })
})

test_that("Data frames may be round-tripped", {
  d <- iris[sample(150,10),]
  s <- derive_table_schema(d)
  tb <- list(url=NULL, tableSchema=s)
  m <- create_metadata(tables=list(tb))
  j <- jsonlite::toJSON(m)

  d_file <- tempfile("data", fileext=".csv")
  m_file <- tempfile("metadata", fileext=".json")

  cat(j, file=m_file)
  write.csv(d, d_file, row.names = FALSE)

  d2 <- read_csvw(d_file, m_file)

  expect_null(d2$error)

  file.remove(d_file, m_file)
})

test_that("Dialect defaults may be overridden", {
  overrides <- list(commentPrefix=NULL, trim=T, foo="bar")
  dialect <- override_defaults(overrides, default_dialect)

  # overrides defaults
  expect_null(dialect$commentPrefix)
  expect_true(dialect$trim)

  # unspecified defaults are retained
  expect_equal(dialect$encoding, "utf-8")

  # newly introduced values are retained
  expect_equal(dialect$foo, "bar")

  # names are unique (doesn't keep default alongside override)
  expect_equal(length(names(dialect)), length(unique(names(dialect))))

  test_that("arbitrary number of fallbacks permitted", {
    dialect <- override_defaults(list(foo=1), list(foo=2), list(foo=3))
    expect_equal(dialect$foo, 1)
  })
})

test_that("Annotation properties may be normalised", {
  orig_op <- options(csvwr_base_uri="http://example.net/")
  on.exit(options(orig_op), add=T, after=F)

  metadata <- list(tables=list(
    list(url="data.csv",
         tableSchema=list(foreignKeys=list(
           list(columnReference="id",
                reference=list(resource="lookup.csv",
                               columnReference="id")))))))
  table <- normalise_metadata(metadata, "data.json")$tables[[1]]

  expect_equal(table$url, "http://example.net/data.csv")
  expect_equal(table$tableSchema$foreignKeys[[1]]$reference$resource, "http://example.net/lookup.csv")
})

test_that("URI Templates may be rendered", {
  # https://w3c.github.io/csvw/syntax/#default-locations-and-site-wide-location-configuration
  expect_equal(render_uri_templates(c("{+url}-metadata.json", "csv-metadata.json"),
                                    url="http://example.org/south-west/devon.csv"),
               c("http://example.org/south-west/devon.csv-metadata.json",
                 "http://example.org/south-west/csv-metadata.json"))

  # last is modified from spec
  expect_equal(render_uri_templates(c("{+url}.json", "csvm.json", "/csvm?file={url}.json"),
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
    expect_equal(strptime(unname(!!e["eg"]), format=!!fmt), as.POSIXlt("2015-03-22"))
  })
})

test_that("lists of lists may be parsed to dataframes", {
  ll <- list(
    list(char="a", logi=T),
    list(num=1),
    list(li=list(foo="bar"))
  )
  d <- data.frame(char=c("a", NA, NA),
                  logi=c(T, NA, NA),
                  num=c(NA, 1, NA),
                  li=I(list(NA, NA, list(foo="bar"))),
                  stringsAsFactors = F)
  expect_equal(list_of_lists_to_df(ll),d)
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
