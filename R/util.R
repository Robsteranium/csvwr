#' Recursive map
#'
#' Applies function `.f` to each element in `.x` as per `purrr::map`.
#' If the value of the element is itself a list, then the function is applied to that in turn.
#' The process is followed recursively until an atomic value at the leaf nodes of the list is found.
#'
#' @param .x a list
#' @param .f a function (called with elements of `.x` as the first argument)
#' @return A list
rmap <- function(.x, .f) {
  m <- function(le) {
    v <- le[[1]]
    r <- if(purrr::vec_depth(v)==1) {
      .f(v)
    } else {
      purrr::lmap(v, m)
    }
    stats::setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}

#' Calculate depth of vector safely
#'
#' Like `purrr::vec_depth` but doesn't attempt to descend into errors
#'
#' @param x a vector
#' @return An integer
#' @md
vec_depth <- function (x)
{
  if (rlang::is_null(x)) {
    0L
  }
  else if (rlang::is_atomic(x)) {
    1L
  }
  else if (inherits(x, "error")) {
    1L
  }
  else if (rlang::is_list(x)) {
    depths <- purrr::map_int(x, vec_depth)
    1L + max(depths, 0L)
  }
  else {
    rlang::abort("`x` must be a vector")
  }
}

#' Recursive lmap
#'
#' Applies function `.f` to each list-element in `.x` as per `purrr::lmap`.
#' If the value of the list-element is itself a list, then the function is applied to that in turn.
#' The process is followed recursively until an atomic value at the leaf nodes of the list is found.
#' If `.f` modifies the name, it is thrown away and replaced by the original name.
#'
#' @param .x a list
#' @param .f a function (called with elements of `.x` as the first argument)
#' @param ... further arguments passed to the function `.f`
#' @return A list
rlmap <- function(.x, .f, ...) {
  m <- function(le) {
    r <- if(vec_depth(le[[1]])==1) {
      .f(le, ...)[[1]]
    } else {
      purrr::lmap(le[[1]], m)
    }
    stats::setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}
