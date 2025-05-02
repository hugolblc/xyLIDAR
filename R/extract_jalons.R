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

   # Correct/adjust coordo
   correct_plot <- BIOMASS::correctCoordGPS(
      longlat = coordinates[, c("typevalue_ddlon", "typevalue_ddlat")],
      coordRel = coordinates[, c("Xrel", "Yrel")],
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
      corner = c(1, 2, 3, 4),
      gridsize = 20, dimX = 100, dimY = 100
   ) %>%
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
                                           "100_100" = '1000_100'
      )
   }

   test = subplot %>%
      st_as_sf(coords = c('XAbs', 'YAbs'))

   a = mapview(test)%>%
      leafem::addStaticLabels(label = test$jalon,
                              noHide = TRUE,
                              direction = 'top',
                              textOnly = TRUE,
                              textsize = "20px")

   print(a)
   return(subplot)
} # Extract all jalons coordinates
