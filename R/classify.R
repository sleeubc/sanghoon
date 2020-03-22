#' classify
#'
#' This function discretize a numeric vector into an interval factor
#'
#' @param x input vector (numeric)
#' @param n number of intervals
#' @param style "equal", "quantile", "jenks"
#' @param num_format a vectorized function that formats numbers.
#' @keywords geom_sf, Jenks, natural breaks, equal interval, quantile, classification
#' @export
#' @examples
#' library(ggplot2)
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +  geom_sf(aes(fill = classify(AREA, 3, style ="jenks", num_format = scales::comma_format(accuracy = 0.01))))

classify <- function(x, n, style="equal", num_format=scales::comma_format()) {

  require(BAMMtools)
  require(scales)
  require(dplyr)

  breaks_cut <- switch( style,
                        equal=seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n+1 ),
                        jenks = getJenksBreaks( x, n+1),
                        quantile = quantile(x, probs = seq(0, 1, 1/n), na.rm=TRUE),
                        stop("Style misspecified.")
  )

  labels_cut <- paste( num_format(breaks_cut)[-length(breaks_cut)], num_format(lead(breaks_cut)[-length(breaks_cut)]), sep=" - ")

  cut( x, breaks= breaks_cut, labels=labels_cut )
}
