#' Find all duplicate rows by chosen columns
#'
#' Returns all duplicate rows including the first occurrence in each duplicate group.
#'
#' @param data a data frame or tibble
#' @param ... columns to check for duplicates
#' @param sort if TRUE (default), sort output by the columns in \code{...}
#' @return A data frame of all rows that have at least one duplicate.
#' @export
#' @examples
#' library(dplyr)
#' iris |> duplicates_by(Sepal.Length, Sepal.Width)

duplicates_by <- function(data, ..., sort = TRUE) {
  selected <- dplyr::select(data, ...)
  dup_tags <- duplicated(selected) | duplicated(selected, fromLast = TRUE)
  if (sort) dplyr::arrange(data[dup_tags, ], ...) else data[dup_tags, ]
}
