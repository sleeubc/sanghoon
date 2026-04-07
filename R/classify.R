#' classify
#'
#' Discretize a numeric vector into an ordered interval factor.
#'
#' @param x input vector (numeric)
#' @param n number of intervals
#' @param style "equal", "quantile", "jenks"
#' @param label a function that formats break values into label strings.
#' @param label_type "observed" labels classes using the minimum and maximum
#'   values observed in each class; "breaks" labels classes using the computed
#'   breakpoints.
#' @export
#' @examples
#' x <- c(1, 1.8, 2, 3, 3.1, 4.5)
#' classify(x, 3, label = scales::number_format(accuracy = 0.1))
#' classify(x, 3, label = scales::number_format(accuracy = 0.1), label_type = "breaks")

classify <- function(x, n, style = "equal", label = scales::comma_format(),
                     label_type = c("observed", "breaks")) {

  label_type <- match.arg(label_type)

  if (all(is.na(x))) stop("`x` contains only NA values; cannot compute breaks.")

  breaks_cut <- switch(style,
    equal    = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1),
    jenks    = BAMMtools::getJenksBreaks(x, n + 1),
    quantile = stats::quantile(x, probs = seq(0, 1, 1 / n), na.rm = TRUE),
    stop("`style` must be one of 'equal', 'quantile', or 'jenks'.")
  )

  breaks_cut <- unique(breaks_cut)
  if (length(breaks_cut) < n + 1) {
    warning("Duplicate break values detected (likely due to ties); fewer than `n` intervals will be produced.")
  }

  lower_breaks <- breaks_cut[-length(breaks_cut)]
  upper_breaks <- breaks_cut[-1]
  labels_breaks <- paste(label(lower_breaks), "to", label(upper_breaks))
  labels_breaks[-1] <- paste0(">", label(lower_breaks[-1]), " to ", label(upper_breaks[-1]))

  if (label_type == "observed") {
    classes <- cut(x, breaks = breaks_cut, labels = FALSE, include.lowest = TRUE)
    labels_cut <- vapply(seq_along(labels_breaks), function(class_i) {
      values <- x[classes == class_i & !is.na(classes)]
      if (length(values) == 0) return(labels_breaks[class_i])

      min_value <- min(values, na.rm = TRUE)
      max_value <- max(values, na.rm = TRUE)
      if (isTRUE(min_value == max_value)) return(label(min_value))

      paste(label(min_value), "to", label(max_value))
    }, character(1))
  } else {
    labels_cut <- labels_breaks
  }

  cut(x, breaks = breaks_cut, labels = labels_cut, include.lowest = TRUE, ordered_result = TRUE)
}
