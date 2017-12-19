#' summarise for each combination of group_by variables
#'
#' This function extends dplyr::summarise to generate summaries for each combination of group_by variables
#'
#' @param data
#' @keywords summarise
#' @export
#' @examples
#' mtcars %>%
#' group_by(cyl, vs) %>%
#' summarise2(cyl_n = n())

summarise2 <- function(data, ...) {
  require(dplyr)

  groupVars <- group_vars(data)

  schemes <-  purrr::map( 0:length(groupVars), ~combn(groupVars, ., simplify=FALSE) ) %>%
    unlist(recursive = FALSE)

  results <-  lapply(schemes, FUN = function(x) {
    do.call(what = group_by_, args = c(list(data), x)) %>%
      summarise(...)
  }
  )

  combined_results <- results %>% bind_rows()
  combined_results[, c(groupVars, setdiff(names(combined_results), groupVars ))]

}
