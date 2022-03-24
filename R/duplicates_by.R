#' Find all duplicate rows by chosen columns.
#'
#' Note that this function generates all duplicate rows including the first one in each duplicate group.
#'
#' @param data a tbl
#' @param ... Variables to check duplicates by.
#' @param sort When sort=TRUE as in the default, the output will be sorted by the variables provided in ...
#' @keywords duplicates
#' @export
#' @examples
#' library(tidyverse)
#' iris %>% duplicates_by(Sepal.Length, Sepal.Width)

duplicates_by <- function(data, ...,  sort=TRUE) {

  selectedCols <- data %>% select(...)

  dupTags <- duplicated(selectedCols) | duplicated(selectedCols, fromLast = TRUE)

  if (sort == TRUE) data[dupTags,] %>% arrange(...)
  else data[dupTags,]
}
