#' classify
#'
#' Discretize a numeric vector into an ordered interval factor.
#'
#' @param x input vector (numeric)
#' @param n number of intervals
#' @param style "equal", "quantile", "jenks"
#' @param label a function that formats break values into label strings.
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) + geom_sf(aes(fill = classify(AREA, 3, style = "jenks", label = scales::comma_format(accuracy = 0.01))))

classify <- function(x, n, style = "equal", label = scales::comma_format()) {

  if (all(is.na(x))) stop("`x` contains only NA values; cannot compute breaks.")

  breaks_cut <- switch(style,
    equal    = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1),
    jenks    = BAMMtools::getJenksBreaks(x, n + 1),
    quantile = quantile(x, probs = seq(0, 1, 1 / n), na.rm = TRUE),
    stop("`style` must be one of 'equal', 'quantile', or 'jenks'.")
  )

  breaks_cut <- unique(breaks_cut)
  if (length(breaks_cut) < n + 1) {
    warning("Duplicate break values detected (likely due to ties); fewer than `n` intervals will be produced.")
  }

  labels_cut <- paste(label(breaks_cut[-length(breaks_cut)]),
                      label(breaks_cut[-1]),
                      sep = " - ")

  cut(x, breaks = breaks_cut, labels = labels_cut, include.lowest = TRUE, ordered_result = TRUE)
}
