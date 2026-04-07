#' Standard error of the mean
#'
#' @param x numeric vector
#' @param na.rm logical; if TRUE, remove NAs before computing. Default FALSE.
#' @return A single numeric value.
#' @export
#' @examples
#' se_mean(c(1, 2, 3, NA), na.rm = TRUE)

se_mean <- function(x, na.rm = FALSE) {
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}
