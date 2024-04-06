#' Function for visualizing positions of dronein 3D space
#'
#' @param pos 3 element numeric vector describing position of drone "x","y","z"
#' @param shooterPos numeric 3 element vector describing position of shooter c(x,y,z)
#' @param shooterA matrix 3x3 describing orientation of shooter, default diag(3)
#'
#' @examples
#' shooterView3d(pos = c(0,0,0), shooterPos = c(0,0,0), shooterA = diag(3))
#'
#'
#' @export shooterView3d
#'
#' @import rgl
#'
shooterView3d <-
  function(pos = c(0, 0, 0),
           shooterPos = c(0, 0, 0),
           shooterA = diag(3),
           type = "points3D") {
    pm <- par(mfrow = c(1, 1))
    par(mar = c(0.5, 0.5, 0.5, 0.5))

    # View angles
    phi = 180
    theta = 0


    rgl::points3d (pos)

    rgl::points3d(
      shooterPos[1],
      shooterPos[2],
      shooterPos[3],
      add = TRUE,
      pch = 23,
      cex = 0.1,
      col = "blue",
      bg = "red"
    )

    #browser()
    xvect = shooterA %*% c(3, 0, 0)
    yvect = shooterA %*% c(0, 2, 0)
    zvect = shooterA %*% c(0, 0, 2)
    # X arrow
    rgl::arrow3d (shooterPos,
                  as.vector(shooterPos + xvect),
                  col = "green")
    # Yarrow
    rgl::arrow3d (shooterPos,
                  as.vector(shooterPos + yvect),
                  col = "red")
    # Z arrow
    rgl::arrow3d (shooterPos,
                  as.vector(shooterPos + zvect),
                  col = "blue")

    # M = diag(4)
    # M[1:3,1:3] = shooterA
    # rgl::view3d(userMatrix = M)
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
# retur(distance)
