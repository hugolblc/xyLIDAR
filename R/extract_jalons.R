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


extract_jalons <- function(coordinates){

   Fieldplot_BDD = coordinates

   # Correct/adjust coordo
   correct_plot <- BIOMASS::correctCoordGPS(
      longlat = Fieldplot_BDD[, c("typevalue_ddlon", "typevalue_ddlat")],
      coordRel = Fieldplot_BDD[, c("Xrel", "Yrel")],
      rangeX = c(0, 100),
      rangeY = c(0, 100),
      drawPlot = TRUE,
      maxDist = 10,
      rmOutliers = TRUE
   )
   # create GPS coordo for all piquets
   subplot <- BIOMASS::cutPlot(
      projCoord = correct_plot$cornerCoords,
      plot = rep("plot", 4),
      corner = c(1, 2, 4, 3),
      gridsize = 20, dimX = 100, dimY = 100
   ) %>%
      dplyr::mutate(sousplot = rep(stringr::str_remove(unique(subplot), 'plot_'), each = 4),
             jalon = paste(XRel,YRel,sep='_')) %>%
      dplyr::select(sousplot, jalon, XRel, YRel, XAbs, YAbs, corner)

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

   return(subplot)
} # Extract all jalons coordinates
