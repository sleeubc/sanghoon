#' Find all duplicate rows in provided columns and sort the rows by the columns.
#'
#' Note that this function provides all duplicate rows including the first one in each duplicate group.
#'
#' @param
#' @keywords duplicates
#' @export
#' @examples
#' library(tidyverse)
#' iris %>% dup_rows(Sepal.Length, Sepal.Width)

dup_rows <- function(data, ...) {

  selectedCols <- data %>% select(...)

  dupTags <- duplicated(selectedCols) | duplicated(selectedCols, fromLast = TRUE)

  data[dupTags,] %>% arrange(...)

}
