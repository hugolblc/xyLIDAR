#' extract_jalons
#'
#' @param coordinates
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#' @importFrom BIOMASS correctCoordGPS
#' @importFrom BIOMASS cutPlot
#' @importFrom forcats fct_recode


extract_jalons <- function(coordinates, type = 1){

   longlat = coordinates[, c("typevalue_ddlon", "typevalue_ddlat")]
   coordRel = coordinates[, c("Xrel", "Yrel")]
   rangeX = c(0, 100)
   rangeY = c(0, 100)
   drawPlot = TRUE
   maxDist = 10
   rmOutliers = TRUE
   projCoord = NULL

   if (is.null(longlat)) {
      stop("Give one set of coordinates: coordinates[, c('typevalue_ddlon', 'typevalue_ddlat')]")
   }

   if (!all(between(coordRel[, 1], lower = rangeX[1], upper = rangeX[2]) &
            between(coordRel[, 2], lower = rangeY[1], upper = rangeY[2]))) {
      stop("coordinates[, c('Xrel', 'Yrel')] must be inside the 0 and 100 ranges")
   }
   if ((!is.null(longlat) && any(dim(longlat) != dim(coordRel))) ||
       (!is.null(projCoord) && any(dim(projCoord) != dim(coordRel)))) {
      stop("GPS and relative coordinates are not of the same dimension")
   }

   if (!is.null(longlat)) {
      projCoord <- latlong2UTM(longlat)
      codeUTM <- unique(projCoord[, "codeUTM"])
      projCoord <- projCoord[, c("X", "Y")]
   }

   res <- procrust(projCoord, coordRel)
   coordAbs <- as.matrix(coordRel) %*% res$rotation
   coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
   dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[,
                                                               2] - projCoord[, 2])^2)
   outliers <- which(dist > maxDist)
   if (length(outliers) == nrow(projCoord)) {
      stop("All coordinates points are considered as outliers at the first stage.\n\n         This may be because some coordinates have very large error associated.\n\n         Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
   }
   if (rmOutliers & length(outliers) > 0) {
      refineCoord <- TRUE
      while (refineCoord) {
         res <- procrust(projCoord[-outliers, ], coordRel[-outliers,
         ])
         coordAbs <- as.matrix(coordRel) %*% res$rotation
         coordAbs <- sweep(coordAbs, 2, res$translation,
                           FUN = "+")
         newdist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 +
                            (coordAbs[, 2] - projCoord[, 2])^2)
         if (all(which(newdist > maxDist) == outliers))
            refineCoord <- FALSE
         outliers <- which(newdist > maxDist)
      }
   }
   cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
   cornerCoord <- cornerCoord[c(1, 2, 4, 3), ]
   cornerCoord <- as.matrix(cornerCoord) %*% res$rotation
   cornerCoord <- sweep(cornerCoord, 2, res$translation, FUN = "+")
   p <- st_multipoint(rbind(cornerCoord, cornerCoord[1, ]))
   ps <- st_polygon(list(p), 1)
   sps <- st_sfc(list(ps))
   if (drawPlot) {
      par(xpd = TRUE, mar = par("mar") + c(0, 0, 0, 7.5))
      plot(if (length(outliers) == 0)
         projCoord
         else projCoord[-outliers, ], col = "grey30", main = "Plot drawing",
         xlim = range(projCoord[, 1], coordAbs[, 1]), ylim = range(projCoord[,
                                                                             2], coordAbs[, 2]), asp = 1, xlab = "X", ylab = "Y",
         axes = FALSE, frame.plot = FALSE)
      usr <- par("usr")
      grid <- sapply(par(c("xaxp", "yaxp")), function(x) {
         seq(x[1], x[2], length.out = x[3] + 1)
      }, simplify = FALSE)
      segments(x0 = grid$xaxp, y0 = usr[3], y1 = usr[4], col = "grey80",
               lty = 1)
      segments(y0 = grid$yaxp, x0 = usr[1], x1 = usr[2], col = "grey80",
               lty = 1)
      axis(side = 1, lty = "blank", las = 1)
      axis(side = 2, lty = "blank", las = 1)
      plot(sps, add = TRUE, lwd = 3)
      points(coordAbs, col = "black", pch = 15, cex = 1.3)
      if (length(outliers) > 0)
         points(projCoord[outliers, ], col = "red", pch = 4,
                cex = 1)
      legend(x = usr[2], y = grid$yaxp[length(grid$yaxp) -
                                          1], c("GPS measurements", ifelse(rmOutliers, "Outliers (discarded)",
                                                                           "Outliers"), "Corrected coord"), col = c("grey30",
                                                                                                                    "red", "black"), pch = c(1, 4, 15, 49), bg = "grey90")
      par(xpd = NA, mar = c(5, 4, 4, 2) + 0.1)
   }
   if (length(outliers) != 0 & !rmOutliers) {
      warning("Be carefull, you may have GNSS measurement outliers. \n",
              "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument).")
   }
   correct_plot <- list(cornerCoords = data.frame(X = cornerCoord[,
                                                            1], Y = cornerCoord[, 2]), correctedCoord = data.frame(X = coordAbs[,
                                                                                                                                1], Y = coordAbs[, 2]), polygon = sps, outliers = outliers)
   if (!is.null(longlat)) {
      correct_plot$codeUTM <- codeUTM
   }

   projCoord = correct_plot$cornerCoords
   plot = rep("plot", 4)
   cornerNum = c(1, 2, 3, 4)
   gridsize = 20
   dimX = 100
   dimY = 100

   if (!(length(dimY) %in% c(1, length(unique(plot))))) {
      stop("Your dimY vector must be of length 1 or of length equal to length(unique(plot))")
   }
   if (any(gridsize > dimX) || any(gridsize > dimY)) {
      stop("Your gridsize is larger than the X or Y dimensions")
   }
   cornerCoord <- data.table(plot = plot, X = projCoord[, 1],
                             Y = projCoord[, 2], cornerNum = cornerNum)
   setnames(cornerCoord, colnames(cornerCoord), c("plot", "X",
                                                  "Y", "cornerNum"))
   cornerCoord <- cornerCoord[order(cornerNum), .SD, by = plot]
   dimRel <- data.table(plot = unique(plot), dimX = dimX, dimY = dimY)
   gridFunction <- function(data, gridsize) {
      absCoordMat <- as.matrix(data[, .(X, Y)])
      plotDimX <- as.numeric(unique(data[, "dimX"]))
      plotDimY <- as.numeric(unique(data[, "dimY"]))
      relCoordMat <- matrix(c(0, 0, 0, plotDimY, plotDimX,
                              plotDimY, plotDimX, 0), byrow = T, ncol = 2)
      gridMat <- as.matrix(expand.grid(X = seq(0, max(relCoordMat[,
                                                                  1]), by = gridsize), Y = seq(0, max(relCoordMat[,
                                                                                                                  2]), by = gridsize)))
      absCoord <- bilinear_interpolation(coord = gridMat,
                                         from_corner_coord = relCoordMat, to_corner_coord = absCoordMat)
      return(data.table(XRel = gridMat[, 1], YRel = gridMat[,
                                                            2], XAbs = absCoord[, 1], YAbs = absCoord[, 2]))
   }
   cornerCoord <- cornerCoord[dimRel, on = "plot"][, gridFunction(.SD,
                                                                  gridsize), by = plot]
   numberingCorner <- function(data) {
      rbindlist(apply(data[XRel < max(XRel) & YRel < max(YRel),
                           -"plot"], 1, function(x) {
                              X <- x["XRel"]
                              Y <- x["YRel"]
                              data[(XRel == X & YRel == Y) | (XRel == X + gridsize &
                                                                 YRel == Y) | (XRel == X + gridsize & YRel ==
                                                                                  Y + gridsize) | (XRel == X & YRel == Y + gridsize),
                                   .(subplot = paste(plot, X/gridsize, Y/gridsize,
                                                     sep = "_"), XRel, YRel, XAbs, YAbs)][, `:=`(cornerNum,
                                                                                                 c(1, 4, 2, 3))]
                           }))
   }
   cornerCoord <- cornerCoord[, numberingCorner(.SD), by = plot,
                              .SDcols = colnames(cornerCoord)]
   subplot <- as.data.frame(cornerCoord) %>%
      dplyr::mutate(sousplot = rep(stringr::str_remove(unique(subplot), 'plot_'), each = 4),
             jalon = paste(XRel,YRel,sep='_')) %>%
      dplyr::select(sousplot, jalon, XRel, YRel, XAbs, YAbs, cornerNum)

   if(type == 1){

      subplot$sousplot <- forcats::fct_recode(subplot$sousplot,
                                              "0_0" = '0_0',
                                              "0_20" = '0_1',
                                              "0_40" = '0_2',
                                              "0_60" = '0_3',
                                              "0_80" = '0_4',
                                              "20_0" = '1_0',
                                              "20_20" = '1_1',
                                              "20_40" = '1_2',
                                              "20_60" = '1_3',
                                              "20_80" = '1_4',
                                              "40_0" = '2_0',
                                              "40_20" = '2_1',
                                              "40_40" = '2_2',
                                              "40_60" = '2_3',
                                              "40_80" = '2_4',
                                              "60_0" = '3_0',
                                              "60_20" = '3_1',
                                              "60_40" = '3_2',
                                              "60_60" = '3_3',
                                              "60_80" = '3_4',
                                              "80_0" = '4_0',
                                              "80_20" = '4_1',
                                              "80_40" = '4_2',
                                              "80_60" = '4_3',
                                              "80_80" = '4_4'
      )
   }

   if(type == 2 ){
      subplot$sousplot <- forcats::fct_recode(subplot$sousplot,
                                              "0_0" = '0_0',
                                              "20_0" = '0_1',
                                              "40_0" = '0_2',
                                              "60_0" = '0_3',
                                              "80_0" = '0_4',
                                              "0_20" = '1_0',
                                              "20_20" = '1_1',
                                              "40_20" = '1_2',
                                              "60_20" = '1_3',
                                              "80_20" = '1_4',
                                              "0_40" = '2_0',
                                              "20_40" = '2_1',
                                              "40_40" = '2_2',
                                              "60_40" = '2_3',
                                              "80_40" = '2_4',
                                              "0_60" = '3_0',
                                              "20_60" = '3_1',
                                              "40_60" = '3_2',
                                              "60_60" = '3_3',
                                              "80_60" = '3_4',
                                              "0_80" = '4_0',
                                              "20_80" = '4_1',
                                              "40_80" = '4_2',
                                              "60_80" = '4_3',
                                              "80_80" = '4_4'
      )

      subplot$jalon <- forcats::fct_recode(subplot$jalon,
                                           "20_0" = '0_20',
                                           "40_0" = '0_40',
                                           "60_0" = '0_60',
                                           "80_0" = '0_80',
                                           "100_0" = '0_100',
                                           "0_20" = '20_0',
                                           "20_20" = '20_20',
                                           "40_20" = '20_40',
                                           "60_20" = '20_60',
                                           "80_20" = '20_80',
                                           "100_20" = '20_100',
                                           "0_40" = '40_0',
                                           "20_40" = '40_20',
                                           "40_40" = '40_40',
                                           "60_40" = '40_60',
                                           "80_40" = '40_80',
                                           "100_40" = '40_100',
                                           "0_60" = '60_0',
                                           "20_60" = '60_20',
                                           "40_60" = '60_40',
                                           "60_60" = '60_60',
                                           "80_60" = '60_80',
                                           "100_60" = '60_100',
                                           "0_80" = '80_0',
                                           "20_80" = '80_20',
                                           "40_80" = '80_40',
                                           "60_80" = '80_60',
                                           "80_80" = '80_80',
                                           "100_80" = '80_100',
                                           "0_100" = '100_0',
                                           "20_100" = '100_20',
                                           "40_100" = '100_40',
                                           "60_100" = '100_60',
                                           "80_100" = '100_80',
                                           "100_100" = '100_100'
      )

      subplot$XRel = as.numeric(str_split(as.character(subplot$jalon), pattern="_", simplify = T)[,1])
      subplot$YRel = as.numeric(str_split(as.character(subplot$jalon), pattern="_", simplify = T)[,2])

   }


   map = mapview(st_as_sf(subplot,coords = c('XAbs', 'YAbs')))%>%
      leafem::addStaticLabels(label = subplot$jalon,
                              noHide = TRUE,
                              direction = 'top',
                              textOnly = TRUE,
                              textsize = "20px")

   print(map)
   return(subplot)
} # Extract all jalons coordinates
