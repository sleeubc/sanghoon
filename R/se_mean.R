#' Standard error of mean, with na.rm option
#'
#' This function calculates the standard error of mean, with na.rm option
#'
#' @param
#' @keywords standard error, mean
#' @export
#' @examples
#' mtcars %>% group_by(cyl, vs) %>% summarise_combo(cyl_n = n(), mean(mpg), se_mean(mpg))

se_mean <- function(x, na.rm=FALSE) {
  if(isTRUE(na.rm)) x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}
