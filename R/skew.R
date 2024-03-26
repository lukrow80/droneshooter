#' Function for computing skew symmetric matrix.
#'
#' @param vector numerical vector c(a,b,c)
#'
#' @examples
#' skew (c(1,2,3))
#'
#' @return
#' 3x3 matrix is returned.
#'
#' @details
#' Useful for computing cross product a x b = skew(a) \%*\% b
#'
#' @export skew

skew <- function(vector) {
  stopifnot(
    is.vector(vector),
    length(vector)==3)

  return (matrix(
    c(0, -vector[3], vector[2],
      vector[3], 0, -vector[1],
      -vector[2], vector[1], 0),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  ))
}
