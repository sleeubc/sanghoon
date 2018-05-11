#' Fitting Linear Models with Clustered Errors
#'
#' This function calculates standard errors with clustered erros and add them to the linear model fit object with a named vector "se_cluster".
#' This vector can be used in stargazer.
#'
#' @param
#' @keywords standard errors with clustered errors
#' @export
#' @examples
#'
#' ## Petersen's data
#' data("PetersenCL", package = "sandwich")
#' m <- lm(y ~ x, data = PetersenCL)
#'
#' ## clustered covariances
#' m1 <- lm_cluster(y ~ x, data = PetersenCL, cluster=firm)
#' summary(m1)
#' m1$se_cluster
#' m1$vcov_cluster
#'
#' stargazer(m, m1, se=list(NULL, m1$se_cluster))

lm_cluster <- function(data, formula, cluster, ...) {

  require(dplyr, sandwich)

  cluster <- enquo(cluster)

  data <- data %>% filter(! is.na(!!cluster))
  fit <- lm(data = data, formula=formula, ...)
  rownum <- fit$model %>% rownames %>% as.numeric()

  vcov_cluster <- sandwich::vcovCL(fit, cluster = data[rownum, quo_name(cluster)])
  se_cluster<-  vcov_cluster %>% diag %>% sqrt

  fit[["vcov_cluster"]] <- vcov_cluster
  fit[["se_cluster"]] <- se_cluster

  fit

}

