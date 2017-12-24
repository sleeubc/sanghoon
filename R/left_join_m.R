#' left_join with messages
#'
#' This function runs left_join and provides information on match type (1:1, 1:m, m:1, m:m) and the number of unmatched rows in the left tibble.
#'
#'
#' @param
#' @keywords left_join
#' @export
#' @examples
#' library(tidyverse)
#' band_members %>% left_join_m(band_instruments)
#' band_members %>% left_join_m(band_instruments, by = "name")
#' band_members %>% left_join_m(band_instruments2, by = c("name" = "artist"))
#'
#' https://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
#'
#' library(dplyr)
#' d1 <- data_frame(
#'   x = letters[2:4],
#'   y = LETTERS[2:4],
#'   a = rnorm(3)
#' )
#'
#' d2 <- data_frame(
#'   x2 = letters[3:1],
#'   y2 = LETTERS[3:1],
#'   b = rnorm(3)
#' )
#'
#' left_join_m(d1, d2, by = c("x" = "x2", "y" = "y2"))



left_join_m <- function(df_left, df_right, by=NULL,... ) {

  if ( is.null(by) ) {
    key_left <- intersect(names(df_left), names(df_right))
    key_right <- key_left
  } else if ( is.null(names(by))) {
    key_left <- by
    key_right <- key_left
  } else {
    key_left <- by %>% names()
    key_right <- by %>% unname()
  }

  outcome_df <- left_join(df_left, df_right, by, ...)

  message(paste0(c("Match type: ",
                   any(duplicated(df_left[, key_left])) %>% ifelse("m","1"),
                   ":",
                   any(duplicated(df_right[, key_right])) %>% ifelse("m","1")
  )))


  unmatched_left <- df_left %>% anti_join(df_right, by) %>% nrow()
  message(paste0("Unmatched rows (left): ", unmatched_left))

  temp <- key_left
  names(temp) <- key_right
  unmatched_right <- df_right %>% anti_join(df_left, by=temp) %>% nrow()
  message(paste0("unmatched_right: ", unmatched_right))

  outcome_df

  }
