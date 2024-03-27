#' Function for visualizing positions of dronein 3D space
#'
#' @param poses numeric array or dataframe with columns names "x","y","z"
#' @param type "points3D", "lines3D" default, "scatter3D"
#' @param shooterPos numeric 3 element vector c(x,y,z)
#' @param shooterA matrix 3x3 describing orientation of shooter, default diag(3)
#'
#' @examples
#' view3d(poses, type = "points3D")
#'
#' @return
#' Numeric distance is returned.
#'
#' @export view3d
#'
#' @import plot3D
#'
view3d <- function(poses, shooterPos = c(0,0,0), shooterA = diag(3), type = "points3D") {
if(is.array(poses)) {poses <- data.frame(poses)}

  pm <- par(mfrow = c(2, 2))
  par(mar=c(0.5,0.5,0.5,0.5))

  # View angles
  phi = c(15, 90, 0, 0)
  theta = c(30, 0, 0, 90)

  for (i in 1:length(phi))
  {

    do.call (type,list(
      poses$x,
      poses$y,
      poses$z,
      pch = 16,
      cex = 1.3,
      col = "black",
      bg = "black",
      lwd = 0.01,
      theta = theta[i],
      phi = phi[i],
      axes = TRUE,
      scale = FALSE,
      xlim = c(-4, 12),
      ylim = c(-4, 4),
      zlim = c(-4, 4),
      d = 1000,
      box = TRUE,
      colkey = list(plot = FALSE)
    ))

    points3D(shooterPos[1],
             shooterPos[2],
             shooterPos[3],
             add = TRUE,
             pch = 23,
             cex = 2,
             col = "blue",
             bg = "red"
    )


    xvect = shooterA %*% c(3,0,0)
    yvect = shooterA %*% c(0,2,0)
    zvect = shooterA %*% c(0,0,2)
    # X arrow
    arrows3D (shooterPos[1],
              shooterPos[2],
              shooterPos[3],
              shooterPos[1]+xvect[1],
              shooterPos[2]+xvect[2],
              shooterPos[3]+xvect[3],
              col = "green",
              add = TRUE)
  # Y arrow
  arrows3D (shooterPos[1],
            shooterPos[2],
            shooterPos[3],
            shooterPos[1]+yvect[1],
            shooterPos[2]+yvect[2],
            shooterPos[3]+yvect[3],
            col = "red",
            add = TRUE)

  arrows3D (shooterPos[1],
            shooterPos[2],
            shooterPos[3],
            shooterPos[1]+zvect[1],
            shooterPos[2]+zvect[2],
            shooterPos[3]+zvect[3],
            col = "blue",
            add = TRUE)
  }
  # stopifnot(
  #   length(p) == 3,
  #   length(u) == 3,
  #   length(p0) == 3,
  #   is.vector(p),
  #   is.vector(u),
  #   is.vector(p0),!identical(u , c(0, 0, 0))
  # )
  #
  # distance = norm(skew(p - p0) %*% u, "F") / norm(as.matrix(u), "F")
  # if (is.nan(distance))
  #   stop ("NaN error")
  # if (is.na(distance))
  #   stop ("NA error")
  # return(distance)
}
