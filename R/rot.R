#' Function for computing basic matrix of rotation
#'
#' @param theta numerical value in rad
#' @param u numeric 3 element vector describing axis of rotation
#' @examples
#' Rx(pi)
#' Ry(0.1)
#' Rz(-0.3)
#' Raxis(c(1,1,1), 0.1)
#'
#' @return
#' 3x3 matrix is returned.
#'
#' @details
#' used in computing basic rotation matrices about x,y,z and u axis
#' @rdname rot
#' @export Rx


Rx <- function(theta) {
  return (matrix(
    c(1, 0, 0,
      0, cos(theta), -sin(theta),
      0, sin(theta), cos(theta)),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  ))
}

#' @rdname rot
#' @export Ry

Ry <- function(theta) {
  return (matrix(
    c(cos(theta), 0, sin(theta),
      0, 1, 0, -sin(theta), 0, cos(theta)),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  ))
}

#' @rdname rot
#' @export Rz

Rz <- function(theta) {
  return (matrix(
    c(cos(theta), -sin(theta), 0,
      sin(theta), cos(theta), 0,
      0, 0, 1),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  ))
}

#' @rdname rot
#' @export Raxis


Raxis <- function(u,theta) {
  u = u/norm(u, type="2")
  A = cos(theta) * diag(3) + sin(theta) * skew(u) + (1 - cos(theta)) * (u %*% t(u))
  return (A)
}
