#' Function for computing distance in 3D space from point p to line described by vector u attached in point p0
#'
#' @param p numeric 3 element vector describing selected point to measure distance to
#' @param u numeric 3 element vector describing direction of the line of sight
#' @param p0 numeric 3 element vector describing point of attachment of vector u
#'
#' @examples
#' dist3d(p,u,p0)
#'
#' @return
#' Numeric distance is returned.
#'
#' @export dist3d

dist3d <- function(p = c(1, 1, 1),
                   u = c(1, 1, 1),
                   p0 = c(0, 0, 0)) {
  stopifnot(
    length(p) == 3,
    length(u) == 3,
    length(p0) == 3,
    is.vector(p),
    is.vector(u),
    is.vector(p0),!identical(u , c(0, 0, 0))
  )

  distance = norm(skew(p - p0) %*% u, "F") / norm(as.matrix(u), "F")
  if (is.nan(distance))
    stop ("NaN error")
  if (is.na(distance))
    stop ("NA error")
  return(distance)
}
