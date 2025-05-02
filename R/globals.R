#' procrust
#'
#' @param X
#' @param Y
#'
#' @export
#'

procrust <- function(X, Y) {
   xmean <- colMeans(X)
   ymean <- colMeans(Y)

   X <- scale(X, scale = FALSE)
   Y <- scale(Y, scale = FALSE)

   XY <- crossprod(X, Y)
   sol <- svd(XY)
   A <- sol$v %*% t(sol$u)

   b <- xmean - ymean %*% A

   return(list(rotation = A, translation = b))
}


latlong2UTM <- function (coord)
{
   coord <- data.table(coord, check.names = TRUE)
   setnames(coord, colnames(coord), c("long", "lat"))
   if (!requireNamespace("proj4")) {
      stop("Please install the package 'proj4'\n\n         \t\tinstall.packages('proj4').")
   }
   codelatlong2UTM <- function(long, lat) {
      Nzone <- (floor((long + 180)/6)%%60) + 1
      Nzone <- paste0(Nzone, ifelse(lat >= 0, " +north ",
                                    " +south "))
      Nzone <- paste0("+proj=utm +zone=", Nzone, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      return(Nzone)
   }
   coord[, `:=`(codeUTM, codelatlong2UTM(long, lat))]
   coord[, `:=`(c("X", "Y"), proj4::project(.(long, lat), proj = unique(.BY))),
         by = codeUTM]
   setDF(coord)
   return(coord)
}
