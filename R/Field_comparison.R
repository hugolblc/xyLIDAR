Field_comparison <- function (Fieldplot_BDD_full, all, directory, plot_name) {


   if(!file.exists(paste0(directory,'/',plot_name,"/Field_scan_comparison"))){
      dir.create(paste0(directory,'/',plot_name,"/Field_scan_comparison"))
   }

   if('position_x' %in% names(Fieldplot_BDD_full) | 'position_y' %in% names(Fieldplot_BDD_full)) {

      field_xy <- Fieldplot_BDD_full$extract %>%
         filter((!(is.na(position_x)) & !(is.na(position_y)))) %>%
         select(ind_num_sous_plot, position_x, position_y) %>%
         rename(id = ind_num_sous_plot) %>%
         mutate(id = as.character(id))

      all <- all %>% left_join(field_xy)

      all_comparison <- all %>% filter((!(is.na(position_x)) & !(is.na(position_y))) | what == 'jalon')

      for (i in 1:length(unique(all_comparison$sousplot))) {

         tmp <- all_comparison %>%
            group_by(id) %>%
            arrange(match(method, c("method1", "method2")), .by_group = TRUE, across(starts_with("method2"))) %>%
            slice(1) %>%
            ungroup() %>%
            filter (sousplot == unique(all_comparison$sousplot)[i])

         tmp_tree <- tmp %>% filter(what == 'tree')
         tmp_jalon <- tmp %>% filter(what == 'jalon')

         my_plot <- ggplot() +
            ggrepel::geom_label_repel(data = tmp_tree, aes(label = id,x=TRUE_X, y=TRUE_Y), size = 5, fill = 'blue', col = 'black') +
            ggrepel::geom_label_repel(data = tmp_tree, aes(label = id,x=position_x, y=position_y), size = 5, fill = 'red', col = 'black') +
            ggrepel::geom_label_repel(data = tmp_jalon, aes(label = id,x=TRUE_X, y=TRUE_Y), size = 5, fill = 'green', col = 'black') +
            theme_classic() +
            theme(
               panel.grid.major = element_line(colour = "black"),
               panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
               panel.background = element_rect(fill = "black")) +
            ggtitle(
               paste0('COMPARISON FIELD AND SCAN :   ', unique(tmp$sousplot)),
               subtitle = 'green : jalon,  blue : LIDAR,   red : manual')

         png(paste0(directory,'/',plot_name,"/Field_scan_comparison/",'Field_scan_comparison_', str_remove(unique(tmp$file),'.csv'),'.png'), width = 600)
         print(my_plot)
         dev.off()
      }

      tmp <- all %>%
         filter((!(is.na(position_x)) & !(is.na(position_y)))) %>%
         filter(!is.nan(TRUE_X))
      x_field <- tmp %>% .[['position_x']] %>% as.numeric()
      y_field <- tmp %>% .[['position_y']] %>% as.numeric()
      x_LIDAR <- tmp %>% .[['TRUE_X']] %>% as.numeric()
      y_LIDAR <- tmp %>% .[['TRUE_Y']] %>% as.numeric()

      coord_field <- as.matrix(cbind(x_field,y_field))
      coord_LIDAR <- as.matrix(cbind(x_LIDAR,y_LIDAR))

      pt_field <-    st_cast(st_sfc(st_multipoint(coord_field)), "POINT")
      pt_LIDAR <- st_cast(st_sfc(st_multipoint(coord_LIDAR)), "POINT")

      distance <- st_distance(pt_field,pt_LIDAR, by_element = TRUE)

      # hist(distance, xlab = 'Distance (m)', main = plot_name)

      png(paste0(directory,'/',plot_name,"/distance_", plot_name,'.png'), width = 600)
      hist(distance, breaks = c(0:45), xlab = 'Distance (m)', main = paste(plot_name, '  |  n = ', length(distance)))
      dev.off()

      tmp_dist <- tmp %>% mutate(distance = distance) %>% select(file, id, distance)

      write.xlsx(tmp_dist, file = paste0(directory,'/',plot_name,"/distance.xlsx"), append = FALSE)

   }else{

      warning("X and Y coordinates should be stored in the position_x & position_y variables")
   }



}



