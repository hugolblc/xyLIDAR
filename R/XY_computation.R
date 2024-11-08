#' XY_computation
#'
#' @param Fieldplot_BDD_full
#' @param list_of_Lidar
#' @param plot_name
#' @param method
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @import grDevices
#' @importFrom BIOMASS latlong2UTM
#' @importFrom ggrepel geom_label_repel




XY_computation <-

   function(
      Fieldplot_BDD_full,
      list_of_Lidar,
      subplot,
      plot_name,
      file_info,
      method ='jalon',
      directory)

   {


      sousplot <- file_info$subplot
      filepath <- file_info$path

# Extract jalons and subplots informations --------------------------------

   x_min <- as.numeric(str_split(sousplot, '_') [[1]] [1])
   x_max <- as.numeric(str_split(sousplot, '_') [[1]] [1]) + 20
   y_min <- as.numeric(str_split(sousplot, '_') [[1]] [2])
   y_max <- as.numeric(str_split(sousplot, '_') [[1]] [2]) + 20


   jalon_theo <-
      expand.grid(seq(x_min, x_max, 20), seq(y_min, y_max, 20)) %>%
      mutate(jalon = paste(Var1, Var2, sep = '_')) %>%
      .[['jalon']]


   adjacent_sousplot <-
      expand.grid(seq(x_min - 20, x_max, 20), seq(y_min - 20, y_max, 20)) %>%
      filter(Var1 >= 0 &
                Var1 < 100 & Var2 >= 0 & Var2 < 100) %>%
      mutate(subplot = paste(Var1, Var2, sep = '_')) %>%
      filter(subplot != sousplot) %>% .[['subplot']]



# Extract data for the subplot --------------------------------------------

   trees_inventory <-
      Fieldplot_BDD_full$extract %>%
      dplyr::filter(sous_plot_name %in% sousplot)


   id_sousplot <-
      trees_inventory %>%
      dplyr::filter (sous_plot_name == sousplot) %>%
      .[['ind_num_sous_plot']]

   id_adj_sousplot <-
      trees_inventory %>%
      dplyr::filter (sous_plot_name %in% adjacent_sousplot) %>%
      .[['ind_num_sous_plot']]

   if(file_info$scan_number == ''){

      tmp <- list_of_Lidar[names(list_of_Lidar) == sousplot][[1]]

   }else{

      tmp <- list_of_Lidar[names(list_of_Lidar) == paste(sousplot,file_info$scan_number, sep='_')][[1]]

   }


   LIDAR_sousplot <-
      tmp %>%

      dplyr::mutate(

         what =  dplyr::case_when(
            stringr::str_detect(id, '_', negate = FALSE) ~ 'jalon',
            TRUE ~ 'tree'
         ),

         file = basename(filepath),

         sousplot = sousplot,

         where = dplyr::case_when(
            (what == 'tree' & id %in% id_sousplot) ~ 'in',
            (what == 'tree' & id %in% id_adj_sousplot) ~ 'adjacent',
            TRUE ~ 'out'
         ),

         where = dplyr::case_when(
            (what == 'jalon' & stringr::str_detect(id, paste(jalon_theo, collapse = '|'))) ~ 'in',
            TRUE ~ where
         ),

         id = case_when(
            stringr::str_detect(id, 'jalon_') ~ stringr::str_remove(id, 'jalon_'),
            stringr::str_detect(id, 'jalon') ~ stringr::str_remove(id, 'jalon'),
            TRUE ~ id
         )
      )%>%

      select (file, sousplot, what, id, where, X_lidar, Y_lidar)


   dup.id <-
      LIDAR_sousplot %>%
      dplyr::filter(duplicated(id)) %>% .[['id']]

   if(method == 'jalon') {

      LIDAR_sousplot <-
         LIDAR_sousplot %>%
         dplyr::mutate(
            duplicated_id = dplyr::case_when(
               id %in% dup.id ~ 'yes',
               TRUE ~ 'no'
            ))

   }

   jalon_ref <-
      dplyr::as_tibble(subplot) %>%
      dplyr::filter(jalon %in% (LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ) %>%
                            .[['id']]) ) %>%
      dplyr::select(jalon, XAbs,YAbs) %>%
      dplyr::group_by(jalon) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()


   jalon_ref <-
      jalon_ref %>%
      dplyr::left_join(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ), by = join_by(jalon == id))


   if(method == 'tree'){

      jalon_ref <-
         jalon_ref %>%
         dplyr::rename(id = jalon) %>%
         dplyr::select(-sousplot)

      tree_ref <- all_jalon_method[(all_jalon_method$id) %in% (LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where %in% c('in','adjacent')) %>% .[['id']]),] %>%
         dplyr::select(id, XAbs, YAbs, file, what, where, X_lidar, Y_lidar)

      all_ref <- rbind(jalon_ref, tree_ref)

      LIDAR_sousplot <- LIDAR_sousplot %>%
         dplyr::mutate(
            ref =
               dplyr::case_when(
                  id %in% all_ref$id ~ 'yes',
                  TRUE ~ 'no'),

            duplicated_id =
               dplyr::case_when(
                  id %in% dup.id ~ 'yes',
                  TRUE ~ 'no'
               )
         )
   }

# Plot title definitiuon --------------------------------------------------


   if(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'out' ) %>% nrow() != 0){

      title = 'PROBLEME WITH JALON'

   } else{title = ''}


   if(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where %in% c('out','adjacent') ) %>% nrow() != 0 ){

      title = paste(title, '| SOME TREES ARE OUT OF THIS SUBPLOT')} else{title = paste(title, '')

      }


   if(TRUE %in% stringr::str_detect(LIDAR_sousplot$duplicated_id, 'yes')){

      title = paste(title, '| SOME ID ARE DUPLICATED')

   } else{title = paste(title, '')}


   if(title == '  '){title = 'NO PROBLEM DETECTED'}


# Plot raw data -----------------------------------------------------------

   my_plot <-
      ggplot2::ggplot(LIDAR_sousplot, ggplot2::aes(x=X_lidar, y=Y_lidar)) +
      {if (method == 'jalon')      ggplot2::geom_label(ggplot2::aes(label = id, fill = what)) } +
      {if (method == 'tree')      ggplot2::geom_label(ggplot2::aes(label = id, fill = ref)) } +
      ggplot2::geom_point(ggplot2::aes(y=Y_lidar+0.000008,shape = where, col = duplicated_id), size = 3) +
      ggplot2::scale_color_manual(values = c('yes' = 'red', 'no' = 'white')) +
      {if (method == 'tree')      ggplot2::scale_fill_manual(values = c('yes' = 'red', 'no' = 'white')) } +
      ggplot2::theme_classic() +
      ggplot2::theme(
         panel.grid.major = element_line(colour = "black"),
         panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
         panel.background = element_rect(fill = "black")) +
      {if (method == 'jalon')      ggplot2::ggtitle(title) } +
      {if (method == 'tree')     ggtitle(paste0(unique(LIDAR_sousplot$file))) }


   if(method == 'jalon'){

      grDevices::png(paste0(directory,'/',plot_name,"/xy_rawdata_method1", '/RAWDATA_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      print(my_plot)
      grDevices::dev.off()

   }

   if(method == 'tree'){

      grDevices::png(paste0(directory,'/',plot_name,"/xy_rawdata_method2", '/RAWDATA_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      print(my_plot)
      grDevices::dev.off()

   }



   # print (paste('####  SOUS PLOT ', sousplot, ' FILE ', unique(LIDAR_sousplot$file),' -------------------'))
   # print ('                                                               ')
   # print (paste('JALON IN                       :', paste(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ) %>% .[['id']], collapse = ', ')))
   # print (paste('JALON OUT OF THIS SUBPLOT      :', paste(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'out' ) %>% .[['id']], collapse = ', ')))
   # print (paste('TREES IN                       :', paste(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where == 'in' ) %>% .[['id']], collapse = ', ')))
   # print (paste('TREES OUT OF THIS SUBPLOT      :', paste(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where == 'out' ) %>% .[['id']], collapse = ', ')))
   # print (paste('TREES ADJACENT OF THIS SUBPLOT :', paste(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where == 'adjacent' ) %>% .[['id']], collapse = ', ')))
   # print (paste('DUPLICATED ID                  :', paste(LIDAR_sousplot %>% dplyr::filter(duplicated(id)) %>% .[['id']] %>% unique(), collapse = ', ')))
   # print (paste('MISSING TREES IN SUBPLOTS      :', paste(missing_trees[stringr::str_detect(names(missing_trees), paste0('^',sousplot))][[1]], collapse = ', ')))
   # print (paste('MISSING TREES IN THIS SCAN     :', paste(id_sousplot[!(id_sousplot %in% (LIDAR_sousplot %>% .[['id']]))], collapse = ', ')))


# Reproject data ----------------------------------------------------------

   projCoord <- BIOMASS::latlong2UTM(LIDAR_sousplot[,c('X_lidar','Y_lidar')])
   codeUTM <- unique(projCoord[, "codeUTM"])
   projCoord <- projCoord[, c("X", "Y")]
   LIDAR_sousplot$X_iPhone = projCoord$X
   LIDAR_sousplot$Y_iPhone = projCoord$Y

   if(method == 'jalon'){

      jalon_ref$LATiPhone = jalon_ref$LONGiPhone = jalon_ref$X_iPhone = jalon_ref$Y_iPhone = NA

      for(j in 1:nrow(jalon_ref)){

         bla <- jalon_ref[j,'jalon'] %>% dplyr::pull()
         jalon_ref$X_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['X_iPhone']]
         jalon_ref$Y_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['Y_iPhone']]

      }

      res <-procrust(jalon_ref[,c("XAbs", "YAbs")], jalon_ref[,c("X_iPhone", "Y_iPhone")])

   }

   if(method == 'tree'){

      all_ref$LATiPhone = all_ref$LONGiPhone = all_ref$X_iPhone = all_ref$Y_iPhone = NA

      for(j in 1:nrow(all_ref)){

         bla <- all_ref[j,'id'] %>% dplyr::pull()
         all_ref$X_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['X_iPhone']]
         all_ref$Y_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['Y_iPhone']]

      }

      res <-procrust(all_ref[,c("XAbs", "YAbs")], all_ref[,c("X_iPhone", "Y_iPhone")])

   }


   coordAbs_allTrees <- as.matrix(LIDAR_sousplot[,c("X_iPhone", "Y_iPhone")]) %*% res$rotation
   coordAbs_allTrees <- sweep(coordAbs_allTrees, 2, res$translation, FUN = "+")
   LIDAR_sousplot$XAbs <- coordAbs_allTrees[,1]
   LIDAR_sousplot$YAbs <- coordAbs_allTrees[,2]



# Plot reprojected data ---------------------------------------------------

   my_plot <-
      ggplot2::ggplot(LIDAR_sousplot) +
      ggrepel::geom_label_repel(ggplot2::aes(label = id,x=XAbs, y=YAbs, col = where, fill = what), size = 5) +
      ggplot2::scale_color_manual(values = c('in' = 'black', 'out' = 'red', 'adjacent' = 'orange')) +
      ggplot2::scale_fill_manual(values = c('tree' = 'lightgreen', 'jalon' = 'white')) +
      ggplot2::ggtitle(sousplot) +
      ggplot2::theme_classic() +
      ggplot2::theme(
         panel.grid.major = element_line(colour = "black"),
         panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
         panel.background = element_rect(fill = "black")) +
      ggplot2::ggtitle(paste0('REPROJECTION FOR THE FILE :   ', unique(LIDAR_sousplot$file)))


   if(method == 'jalon'){

      grDevices::png(paste0(directory,'/',plot_name,"/xy_reproject_method1", '/REPROJECT_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      print(my_plot)
      grDevices::dev.off()

   }

   if(method == 'tree'){

      grDevices::png(paste0(directory,'/',plot_name,"/xy_reproject_method2", '/REPROJECT_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      print(my_plot)
      grDevices::dev.off()

   }



   LIDAR_sousplot <-
      LIDAR_sousplot %>%
      dplyr::mutate(
         plot = plot_name,
         method = method) %>%
      { if (method == 'jalon') dplyr::mutate(., n_jalon_ref = length(unique(jalon_ref$jalon)) ) else . }  %>%
      { if (method == 'tree') dplyr::mutate(., n_jalon_ref = length(unique(jalon_ref$id)) ) else . }  %>%
      { if (method == 'jalon') dplyr::mutate(., n_tree_ref = 0 ) else . }  %>%
      { if (method == 'tree') dplyr::mutate(., n_tree_ref = length(unique(tree_ref$id)) ) else . } %>%
      dplyr::mutate(n_tot_ref = n_jalon_ref + n_tree_ref ) %>%
      { if (method == 'tree') dplyr::select(., -ref) else . }

   return(LIDAR_sousplot)

}
