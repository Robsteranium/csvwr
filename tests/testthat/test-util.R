# value is:
# - w1 atomic vector
# - x1 unnamed list
# - y1 named list
# - z1 nested list (named and unnamed)

lowercase <- list(w1="a",x1=list("b","b"),y1=list(y2="c"),z1=list(z2=list("d"), list(list(list("e")))))
uppercase_values <- list(w1="A",x1=list("B","B"),y1=list(y2="C"),z1=list(z2=list("D"), list(list(list("E")))))

test_that("rmap", {
  expect_equal(rmap(lowercase, stringr::str_to_upper),
               uppercase_values)
})

test_that("rlmap values", {
  upcase_values <- function(le) {
    r <- stringr::str_to_upper(le[[1]])
    setNames(list(r), names(le))
  }

  expect_equal(rlmap(lowercase, upcase_values),
               uppercase_values)
})

test_that("rlmap doesn't attempt to map into errors", {
  error_list <- tryCatch(stop("oops"), error=function(e) { list(list(error=e)) })
  expect_equal(rlmap(error_list, normalise_property),
               error_list)
})
#uppercase_values_and_names <- list(W1="A",X1=list("B","B"),Y1=list(y2="C"),Z1=list(Z2=list("D"), list("E")))

# test_that("rlmap names and values", {
#   upcase_names_and_values <- function(le) {
#     r <- stringr::str_to_upper(le[[1]])
#     n <- stringr::str_to_upper(names(le))
#     setNames(list(r), n)
#   }
#
#   expect_equal(rlmap(lowercase, upcase_names_and_values),
#                uppercase_values_and_names)
# })

test_that("vec_depth doesn't attempt to measure the depth of errors", {
  err <- tryCatch(stop("oops"), error=function(e) {e})
  expect_error(purrr::vec_depth(err))
  expect_equal(vec_depth(err), 1)
})

