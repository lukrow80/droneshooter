#' Function for generating drone position in tine and 3D space
#'
#' @param time time vector e.g. t= 1:100
#' @param xfun yfun, zfun - functions generating points in time
#'
#' @examples
#' dronePos(t,xfun,yfun,zfun)
#'
#' @return
#' return an array of x,y,z positions of drone
#'
#' @details
#' an example of funciton: xfun = function(time) 5 + 2*sin(time)
#'
#' @export dronePos

dronePos <- function(time,
                     xfun = function(time) 100 + sin(time),
                     yfun = function(time) 20 + 3 * sin(time),
                     zfun = function(time) 80 + 1 * sin(time),
                     timeunits = "s",
                     dimunits = "m"){
  x <- xfun(time)
  y <- yfun(time)
  z <- zfun(time)
  positions <- cbind(time, x, y, z)
  attr(positions, "time units") <- timeunits
  attr(positions, "dimensions units") <- dimunits
  return(positions)
}
