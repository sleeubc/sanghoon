#' Run summarise for all combinations of group_by variables
#'
#' Runs \code{dplyr::summarise} for every subset combination of the active
#' \code{group_by} variables and binds the results together.
#'
#' @param data a grouped data frame (from \code{dplyr::group_by})
#' @param ... summarise expressions passed to \code{dplyr::summarise}
#' @param type if TRUE, adds a \code{.type.} integer column identifying each grouping combination
#' @param na.str if TRUE (default), converts NA group values to the string \code{"NA"},
#'   distinguishing "not grouped by this variable" from truly missing values
#' @return A tibble with one block of rows per grouping combination, bound together.
#' @export
#' @examples
#' library(dplyr)
#' data <- tibble(a = c("a1","a1","a2","a3"), b = c("b1","b2","b2",NA), c = c(1,2,3,4))
#' data |> group_by(a, b) |> summarise_combo(n(), mean(c))
#' data |> group_by(a, b) |> summarise_combo(n(), mean(c), type = TRUE)
#' data |> group_by(a, b) |> summarise_combo(n(), mean(c), type = TRUE, na.str = FALSE)

summarise_combo <- function(data, ..., type = FALSE, na.str = TRUE) {

  group_vars_str   <- dplyr::group_vars(data)
  group_vars_names <- purrr::map(group_vars_str, as.name)

  group_combos <- purrr::map(0:length(group_vars_names),
                             ~ combn(group_vars_names, .x, simplify = FALSE)) |>
    unlist(recursive = FALSE)

  if (isTRUE(na.str)) {
    results <- purrr::map(group_combos, function(grp) {
      replace_na_list <- setNames(rep(list("NA"), length(grp)), as.character(grp))
      data |> dplyr::group_by(!!!grp) |> dplyr::summarise(...) |>
        tidyr::replace_na(replace_na_list)
    })
  } else {
    results <- purrr::map(group_combos, function(grp) {
      data |> dplyr::group_by(!!!grp) |> dplyr::summarise(...)
    })
  }

  if (isTRUE(type)) {
    results <- purrr::map2(results, seq_along(results),
                           ~ dplyr::mutate(.x, .type. = .y) |>
                             dplyr::select(.type., dplyr::everything()))
  }

  dplyr::bind_rows(results) |> dplyr::select(!!!group_vars_names, dplyr::everything())
}
