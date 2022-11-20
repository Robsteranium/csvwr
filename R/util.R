#' Recursive map
#'
#' Applies function `.f` to each element in `.x` as per `purrr::map`.
#' If the value of the element is itself a list, then the function is applied to that in turn.
#' The process is followed recursively until an atomic value at the leaf nodes of the list is found.
#'
#' @param .x a list
#' @param .f a function (called with elements of `.x` as the first argument)
#' @return A list
#' @keywords internal
rmap <- function(.x, .f) {
  m <- function(le) {
    v <- le[[1]]
    r <- if(purrr::pluck_depth(v)==1) {
      .f(v)
    } else {
      purrr::lmap(v, m)
    }
    stats::setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}


#' Identify csvw properties
#'
#' Overrides the default behaviour in `purrr::pluck_depth` so that we don't descend into errors
#'
#' @param x a vector
#' @return A boolean
#' @md
#' @keywords internal
is_property <- function(x) {
  (is.expression(x) || is.list(x)) & !inherits(x, "error")
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
#' @keywords internal
rlmap <- function(.x, .f, ...) {
  m <- function(le) {
    r <- if(purrr::pluck_depth(le[[1]], is_node=is_property)<=1) {
      .f(le, ...)[[1]]
    } else {
      purrr::lmap(le[[1]], m)
    }
    stats::setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}
