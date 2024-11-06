#' save_gpkg
#'
#' @param all
#' @param crs
#'
#' @export
#' @import dplyr
#' @import sf
#'

save_gpkg <-

   function(all, crs, sub_plot){

   unique_id <- all %>%
      dplyr::filter(what == 'tree') %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      dplyr::group_by(id) %>%
      arrange(match(method, c("method1", "method2")), .by_group = TRUE, dplyr::across(dplyr::starts_with("method2"))) %>%
      slice(1)

   all_sf = sf::st_as_sf(as.data.frame(all), coords = c("XAbs", "YAbs"), crs = crs, agr = "constant")
   unique_id_sf = sf::st_as_sf(as.data.frame(unique_id), coords = c("XAbs", "YAbs"), crs = crs, agr = "constant")

   sf::st_write(all_sf, paste0(directory,'/',plot_name,'/',plot_name,"_all_project_trees",'.gpkg'))
   sf::st_write(unique_id_sf, paste0(directory,'/',plot_name,'/',plot_name,"_unique_id_sf",'.gpkg'))
   sf::st_write(sub_plot, paste0(directory,'/',plot_name,'/',plot_name,"_subplot",'.gpkg'))

} # Save .gpkg
