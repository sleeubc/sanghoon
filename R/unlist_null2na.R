#' unlist_null2na
#'
#' This function converts NULL to NA and then apply unlist().
#' Inspired by https://stackoverflow.com/questions/2991514/prevent-unlist-to-drop-null-values
#'
#' @param x input list
#' @param ... passed to unlist()
#' @keywords unlist, NULL, NA
#' @export
#' @examples
#' x <- list(NULL, 1, 2)
#' unlist_null2na(x)

unlist_null2na <- function(l, ...) {

  l[sapply(l, function(x) length(x)==0L)] <- NA
  unlist(l, ...)

  }

