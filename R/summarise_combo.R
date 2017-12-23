#' Run 'dplyr::summarise' for each combination of group_by variables
#'
#' This function runs dplyr::summarise for each combination of group variables.
#'
#' https://stackoverflow.com/questions/28992028/grouping-over-all-possible-combinations-of-several-variables-with-dplyr/47949800#47949800
#'
#' @param
#' @keywords summarise
#' @export
#' @examples
#' library(tidyverse)
#' mtcars %>% group_by(cyl, vs) %>% summarise_combo(cyl_n = n(), mean(mpg))

summarise_combo <- function(data, ...) {

  groupVars <- group_vars(data) %>% map(as.name)

  groupCombos <-  map( 0:length(groupVars), ~combn(groupVars, ., simplify=FALSE) ) %>%
    unlist(recursive = FALSE)

  results <- groupCombos %>%
    map(function(x) {data %>% group_by(!!! x) %>% summarise(...)} ) %>%
    bind_rows()

  results %>% select(!!! groupVars, everything())
}

