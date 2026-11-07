#' Estimate Chain Ladder Factors and Reserve Amounts
#'
#' @param triangle A cumulative run-off matrix produced by make_triangle().
#' @return A list containing link ratios, completed triangle, and IBNR reserves.
#' @export
fit_chain_ladder <- function(triangle) {
  n <- ncol(triangle)
  factors <- numeric(n - 1)

  for (j in 1:(n - 1)) {
    rows <- which(!is.na(triangle[, j + 1]) & triangle[, j + 1] > 0)
    factors[j] <- sum(triangle[rows, j + 1]) / sum(triangle[rows, j])
  }

  proj_triangle <- triangle
  for (i in 1:nrow(proj_triangle)) {
    for (j in 2:n) {
      if (is.na(proj_triangle[i, j]) || proj_triangle[i, j] == 0) {
        proj_triangle[i, j] <- proj_triangle[i, j - 1] * factors[j - 1]
      }
    }
  }

  latest <- apply(triangle, 1, function(row) tail(row[row > 0], 1))
  ultimate <- proj_triangle[, n]
  ibnr <- ultimate - latest

  list(factors = factors, full_triangle = proj_triangle, ibnr_reserve = ibnr)
}
