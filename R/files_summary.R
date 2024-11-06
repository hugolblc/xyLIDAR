#' files_summary
#'
#' @param root_in
#' @param export
#'
#' @export
#'
#' @import dplyr
#' @import stringr

files_summary <- function(root_in, export = FALSE, directory = NULL, plot_name = NULL){


   List_file = dir(root_in)
   List_path = dir(root_in, full.names = T)
   list_of_Lidar <- list()


   for (i in 1 : length(List_path)) {

      tmp <- readLines(List_path[i]) %>% stringr::str_replace_all(',',';')

      data_type <- if_else('Notes2' %in% tmp[1], 'type2','type1')

      if( data_type == 'type2'){

         tmp <- stringr::str_split(tmp,';', simplify = T) %>%
            dplyr::as_tibble() %>%
            dplyr::slice(-1) %>%
            dplyr::rename(id = V9,
                   X_lidar = V3,
                   Y_lidar = V2) %>%
            dplyr::select(id, X_lidar, Y_lidar) %>%
            dplyr::mutate(X_lidar = as.numeric(X_lidar),
                   Y_lidar = as.numeric(Y_lidar)) %>%
            dplyr::filter(id != '')
      } else {

         tmp <- stringr::str_split(tmp,';', simplify = T) %>%
            dplyr::as_tibble() %>%
            dplyr::slice(-1) %>%
            dplyr::rename(id = V8,
                   X_lidar = V3,
                   Y_lidar = V2) %>%
            dplyr::select(id, X_lidar, Y_lidar) %>%
            dplyr::mutate(X_lidar = as.numeric(X_lidar),
                   Y_lidar = as.numeric(Y_lidar)) %>%
            dplyr::filter(id != '')
      }

      list_of_Lidar[[i]] <- tmp

      if(length(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]]) == 5){
         names(list_of_Lidar)[i] <- paste(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]][3:5], collapse = '_')
      }

      if(length(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]]) == 4){
         names(list_of_Lidar)[i] <- paste(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]][3:4], collapse = '_')
      }

   }

   for (i in 1:length(list_of_Lidar)) {

      tmp_tree <- list_of_Lidar[[i]] %>% dplyr::filter(stringr::str_detect(id, '_', negate = TRUE)) %>% .[['id']] %>% unique() %>% length()
      tmp_jalon <- list_of_Lidar[[i]] %>% dplyr::filter(stringr::str_detect(id, '_', negate = FALSE)) %>% .[['id']] %>% unique() %>% length()

      if(i == 1){nb_tree <- tmp_tree ; nb_jalon <- tmp_jalon } else{nb_tree <- c(nb_tree,tmp_tree) ; nb_jalon <- c(nb_jalon,tmp_jalon)}
   }

   file_summary <-

      List_file %>%
      stringr::str_remove('.csv') %>%
      stringr::str_split('_', simplify = TRUE) %>%
      {if (ncol(.) == 4) `colnames<-`(.,c('site','plot_number','x','y')) else `colnames<-`(.,c('site','plot_number','x','y','scan_number'))} %>%
      dplyr::as_tibble() %>%
      {if (ncol(.) == 4) dplyr::mutate(.,scan_number = NA_character_) else .} %>%
      dplyr::mutate(subplot = paste(x,y,sep='_'),
             nb_tree = nb_tree,
             nb_jalon = nb_jalon,
             path = List_path) %>%
      dplyr::arrange(x,y)

   return(list(file_summary = file_summary,list_of_Lidar = list_of_Lidar))

}
