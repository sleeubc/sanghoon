#' left_join with match diagnostics
#'
#' Runs \code{dplyr::left_join} and prints the match type (1:1, 1:m, m:1, m:m)
#' and the number of unmatched rows from \code{x}.
#'
#' @param x left data frame
#' @param y right data frame
#' @param by a character vector of column names to join by, optionally named
#'   (\code{c("left_col" = "right_col")}). If NULL, joins on all shared column names.
#' @param ... additional arguments passed to \code{dplyr::left_join}
#' @return The result of \code{dplyr::left_join(x, y, by, ...)}.
#' @export
#' @examples
#' library(dplyr)
#' band_members |> left_join_m(band_instruments)
#' band_members |> left_join_m(band_instruments, by = "name")
#' band_members |> left_join_m(band_instruments2, by = c("name" = "artist"))

left_join_m <- function(x, y, by = NULL, ...) {

  # Derive key column names without relying on unexported dplyr internals
  if (is.null(by)) {
    key_x <- key_y <- intersect(names(x), names(y))
  } else {
    key_y <- unname(by)
    nms   <- names(by)
    key_x <- if (!is.null(nms)) ifelse(nms != "", nms, by) else by
  }

  outcome      <- suppressMessages(dplyr::left_join(x, y, by, ...))
  unmatched    <- suppressMessages(dplyr::anti_join(x, y, by) |> nrow())

  message("Match type: ",
          ifelse(anyDuplicated(x[key_x]), "m", "1"), ":",
          ifelse(anyDuplicated(y[key_y]), "m", "1"))
  message("Number of rows: ", nrow(x), " became ", nrow(outcome),
          ", with ", unmatched, " unmatched rows.")

  outcome
}
