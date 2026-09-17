#' @useDynLib GDstepper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Fit a linear regression using Gradient Descent
#' @param x numeric vector
#' @param y numeric vector
#' @param alpha learning rate
#' @param iterations number of iterations
#' @export
fit_gd <- function(x, y, alpha = 0.01, iterations = 1000) {

  if (length(x) != length(y)) {
    stop("length of x and y must be the same")
  }

  res <- gd_cpp(x, y, alpha, iterations)
  class(res) <- "gradr_fit"

  return(res)
}

#' @export
print.gradr_fit <- function(x, ...) {
  cat("=== Gradient Descent Fit ===\n")
  cat("Slope (m):    ", round(x$slope, 4), "\n")
  cat("Intercept (b):", round(x$intercept, 4), "\n")
}
