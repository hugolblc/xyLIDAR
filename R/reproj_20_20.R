
reproj_20_20 <- function (all, directory, plot_name) {

   dir.create(paste0(directory,'/',plot_name,"/20_20_PROJECTION"))


      for (i in 1:length(unique(all$file))) {

         tmp <- all %>% filter(file == unique(all$file)[i])
         jalon_ref <- subplot %>% filter(sousplot == unique(tmp$sousplot))
         jalon_ref <- jalon_ref[as.character(jalon_ref$jalon) %in% tmp$id,]

         jalon_ref <-jalon_ref %>% left_join(tmp %>% filter(what == 'jalon' & where == 'in' ) %>% select(-c("XAbs", "YAbs")), by = join_by(jalon == id))

         res <-procrust((jalon_ref[,c("XRel", "YRel")]), jalon_ref[,c("XAbs", "YAbs")])


         coordAbs_allTrees <- as.matrix(tmp[,c("XAbs", "YAbs")]) %*% res$rotation
         coordAbs_allTrees <- sweep(coordAbs_allTrees, 2, res$translation, FUN = "+")
         tmp$TRUE_X <- coordAbs_allTrees[,1]
         tmp$TRUE_Y <- coordAbs_allTrees[,2]

         TRUE_XY <- tmp
         assign(  paste("TRUE_XY_20_20", unique(TRUE_XY$file), sep = "_"), TRUE_XY )

      }

      list_of_objects <- mget(ls(pattern="TRUE_XY_20_20"))

      # bind the elements together into a data frame

      all <- do.call("rbind", list_of_objects)

      all$quadrat_x <- as.numeric(str_split(all$sousplot, '_', simplify = TRUE)[,1])
      all$quadrat_y <- as.numeric(str_split(all$sousplot, '_', simplify = TRUE)[,2])

      all <- all %>%

         mutate(
            TRUE_X_20 = TRUE_X - quadrat_x,
            TRUE_Y_20 = TRUE_Y - quadrat_y) %>%

         mutate(
            quadrat_x_mesured = case_when(
               TRUE_X < 20 ~ 0,
               TRUE_X > 20 & TRUE_X < 40 ~ 20,
               TRUE_X > 40 & TRUE_X < 60 ~ 40,
               TRUE_X > 60 & TRUE_X < 80 ~ 60,
               TRUE_X > 80 ~ 80

            ),

            quadrat_y_mesured = case_when(
               TRUE_Y < 20 ~ 0,
               TRUE_Y > 20 & TRUE_Y < 40 ~ 20,
               TRUE_Y > 40 & TRUE_Y < 60 ~ 40,
               TRUE_Y > 60 & TRUE_Y < 80 ~ 60,
               TRUE_Y > 80 ~ 80

            ),

            quadrat_mesured = paste(quadrat_x_mesured,quadrat_y_mesured,sep = '_'),

            quadrat_check = case_when(
               quadrat_mesured == sousplot & what != 'jalon' ~ TRUE,
               quadrat_mesured != sousplot & what != 'jalon' ~ FALSE,
               TRUE ~ NA
            )) %>%
         select(-c(quadrat_x_mesured,quadrat_y_mesured)) %>%
         print(n=100)

      for (i in 1:length(unique(all$sousplot))){


         tmp <- all %>% filter(sousplot == unique(all$sousplot)[i])

         my_plot <- ggplot(tmp) +
            ggrepel::geom_label_repel(aes(label = id,x=TRUE_X_20, y=TRUE_Y_20, col = where, fill = what), size = 5) +
            scale_color_manual(values = c('in' = 'black', 'out' = 'red', 'adjacent' = 'orange')) +
            scale_fill_manual(values = c('tree' = 'lightgreen', 'jalon' = 'white')) +
            ggtitle(unique(tmp$sousplot)) +
            theme_classic() +
            theme(
               panel.grid.major = element_line(colour = "black"),
               panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
               panel.background = element_rect(fill = "black")) +
            ggtitle(paste0('REPROJECTION FOR THE SUBPLOT :   ', unique(tmp$sousplot)))

         png(paste0(directory,'/',plot_name,"/20_20_PROJECTION/",'20_20_PROJECTION_', str_remove(unique(tmp$file),'.csv'),'.png'), width = 600)
         print(my_plot)
         dev.off()
      }

      return(all)
}





