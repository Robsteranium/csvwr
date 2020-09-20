#' Recursive map
#'
#' Applies function `.f` to each element in `.x` as per `purrr::map`.
#' If the value of the element is itself a list, then the function is applied to that in turn.
#' The process is followed recursively until an atomic value at the leaf nodes of the list is found.
rmap <- function(.x, .f) {
  m <- function(le) {
    v <- le[[1]]
    r <- if(purrr::vec_depth(v)==1) {
      .f(v)
    } else {
      purrr::lmap(v, m)
    }
    setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}

#' Recursive lmap
#'
#' Applies function `.f` to each list-element in `.x` as per `purrr::lmap`.
#' If the value of the list-element is itself a list, then the function is applied to that in turn.
#' The process is followed recursively until an atomic value at the leaf nodes of the list is found.
#' If `.f` modifies the name, it is thrown away and replaced by the original name.
rlmap <- function(.x, .f, ...) {
  m <- function(le) {
    r <- if(purrr::vec_depth(le[[1]])==1) {
      .f(le, ...)[[1]]
    } else {
      purrr::lmap(le[[1]], m)
    }
    setNames(list(r), names(le))
  }

  purrr::lmap(.x, m)
}
