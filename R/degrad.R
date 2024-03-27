#' Function for transforming deg2rad and rad2deg
#'
#' @param deg numerical
#' @param rad numerical
#'
#' @examples
#' deg2rad (30)
#' rad2deg (pi/2)
#'
#' @return
#' numeric value
#'
#' @rdname convert
#' @export deg2rad

deg2rad <- function(deg) {
  return (deg * pi / 180)
}

#' @rdname convert
#' @export rad2deg

rad2deg <- function(rad) {
  return (rad * 180 / pi)
}

