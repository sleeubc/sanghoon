#' Run 'dplyr::summarise' for each combination of group_by variables
#'
#' This function runs dplyr::summarise for each combination of group variables.
#'
#' https://stackoverflow.com/questions/28992028/grouping-over-all-possible-combinations-of-several-variables-with-dplyr/47949800#47949800
#'
#' @param data data frame or tibble
#' @param ... summarise commands
#' @param type  If TRUE, it adds "_type_" column, an identifier for each group.
#' @param na.str If TRUE, it convers NA to a string "NA" within each group. This distinguished "NA" as a group variable value from NA indicating the variable was not used as a group variable. See Examples.
#' @keywords summarise, combo
#' @export
#' @examples
#' library(tidyverse)
#' data <- tibble(a = c("a1","a1","a2","a3"), b= c("b1", "b2", "b2", NA), c=c(1,2,3,4) )
#' data %>% group_by(a,b) %>% summarise_combo(n(), mean(c))
#' data %>% group_by(a,b) %>% summarise_combo(n(), mean(c), type=TRUE)
#' data %>% group_by(a,b) %>% summarise_combo(n(), mean(c), type=TRUE, na.str=FALSE)

summarise_combo <- function(data, ..., type=FALSE, na.str=TRUE) {

  groupVarsStr <- group_vars(data)
  groupVarsNames <- groupVarsStr %>% map(as.name)

  groupCombos <-  map( 0:length(groupVarsNames), ~combn(groupVarsNames, ., simplify=FALSE) ) %>%
    unlist(recursive = FALSE)


  if (isTRUE(na.str)) {

    results <- groupCombos %>%
      map(function(x) {
        replace_na_list <- rep("NA", length(x)) %>% as.list() %>% setNames(x)
        data %>% group_by(!!! x) %>% summarise(...) %>% replace_na(replace_na_list)
      } )


  } else {

    results <- groupCombos %>%
      map(function(x) {
        data %>% group_by(!!! x) %>% summarise(...)
      } )

  }

  if (isTRUE(type)) results <- results %>% map2( 1:length(results), ~ .x %>% mutate(`_type_` = .y) %>% select(`_type_`, everything())  )

  results  %>% bind_rows() %>% select(!!! groupVarsNames, everything())

}

