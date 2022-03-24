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
#' # The following is based on https://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
#'
#' library(dplyr)
#'
#' d1 <- tibble(
#'   x = letters[1:3],
#'   y = LETTERS[1:3],
#'   a = rnorm(3)
#' )
#'
#' d2 <- tibble(
#'   x2 = c("a","b","b"),
#'   y2 = c("B"),
#'   b = rnorm(3)
#' )
#'
#' left_join_m(d1, d2, by = c("x" = "x2", "y" = "y2"))
#'
#' d1 <- tibble(
#'   x = c("a","b","b"),
#'   y = c("B"),
#'   a = rnorm(3)
#' )
#'
#' d2 <- tibble(
#'   x2 = letters[1:3],
#'   y2 = LETTERS[1:3],
#'   b = rnorm(3)
#' )
#'
#' left_join_m(d1, d2, by = c("x" = "x2", "y" = "y2"))

left_join_m <- function(x, y, by=NULL,... ) {

  suppressMessages(
    by2  <-  common_by(by, x, y)
  )

  outcome <- left_join(x, y, by,...)

  message(paste0(c("Match type: ",
                   any(duplicated(x[, by2$x])) %>% ifelse("m","1"),
                   ":",
                   any(duplicated(y[, by2$y])) %>% ifelse("m","1")
  )))

  suppressMessages(
    unmatched_rows <- x %>% anti_join(y, by) %>% nrow()
  )

  message(c(
    "Number of rows: ",
    nrow(x),
    " became ",
    nrow(outcome)),
    ", with ",
    unmatched_rows,
    " unmatched rows."
  )

  outcome

  }
