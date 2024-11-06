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
