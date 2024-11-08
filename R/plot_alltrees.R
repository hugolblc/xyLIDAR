#' plot_alltrees
#'
#' @param Fieldplot_BDD_full
#' @param all
#' @param subplot
#' @param crs
#'
#' @export
#' @import dplyr
#' @import sf
#'

plot_alltrees <-

   function(
      Fieldplot_BDD_full,
      all,
      subplot,
      crs,
      col)

      {

   id_ssplot <-
      Fieldplot_BDD_full$extract %>%
      dplyr::select(ind_num_sous_plot,sous_plot_name) %>%
      dplyr::rename(id = ind_num_sous_plot)


   id_ssplot$id <- as.character(id_ssplot$id)


   all <-
      all %>%
      dplyr::left_join(id_ssplot)


   all <-
      all %>%
      dplyr::mutate(sous_plot_name = dplyr::case_when(
         is.na(sous_plot_name) & what == 'tree' ~ 'tree not in inventory data',
         is.na(sous_plot_name) & what == 'jalon' ~ 'jalon',
         TRUE ~ sous_plot_name
      ))


   subplot_sf <-
      subplot %>%
      dplyr::group_by(sousplot) %>%
      dplyr::summarise(sousplot = unique(sousplot),
                X1 = XAbs[1],
                X2 = XAbs[2],
                X3 = XAbs[4],
                X4 = XAbs[3],
                X5 = XAbs[1],
                Y1 = YAbs[1],
                Y2 = YAbs[2],
                Y3 = YAbs[4],
                Y4 = YAbs[3],
                Y5 = YAbs[1])

   for (i in 1:25) {

      tmp  <-
         sf::st_polygon(
         list(
            rbind(
               c(X = subplot_sf$X1[i],Y = subplot_sf$Y1[i]),
               c(X = subplot_sf$X2[i],Y = subplot_sf$Y2[i]),
               c(X = subplot_sf$X3[i],Y = subplot_sf$Y3[i]),
               c(X = subplot_sf$X4[i],Y = subplot_sf$Y4[i]),
               c(X = subplot_sf$X1[i],Y = subplot_sf$Y1[i])
            )
         )
      )

      assign (paste('tttttt',i,sep = "_"), tmp)

   }

   nrows <- 25

   sub_plot <- sf::st_sf(crs = crs,
                     sous_plot_name = 1:nrows,
                     geometry = sf::st_sfc(lapply(1:nrows,
                                              function(x) sf::st_geometrycollection())
                     )
   ) # Create a fake multipolygons of 25 object with the good crs

   # Add the right 25 polygon geometry
   for (j in 1:25) {sub_plot$geometry[j] <- mget(ls(pattern = "tttttt"))[[j]]}

   myplot <- ggplot2::ggplot(all ) +
      ggplot2::geom_sf(data = sub_plot, fill = 'black') +
      ggplot2::geom_point(aes(x=XAbs, y=YAbs, col = sous_plot_name, size = what)) +
      ggplot2::scale_size_manual(values = c('jalon' = 5,
                                            'tree' = 3)) +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::facet_wrap(~method) +
      ggplot2::ggtitle(plot_name) +
      ggplot2::theme_bw() +
      ggplot2::theme_classic()


   png(paste0(directory,'/',plot_name,'/all_projected_trees.png'), width = 600)

   print(myplot)

   dev.off()

   print(paste0('PLOT SAVE IN :', directory,'/',plot_name,"/all_projected_trees.jpg"))


   return(sub_plot)

} # Plots all trees in the plot
