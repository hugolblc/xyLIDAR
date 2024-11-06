#' import_plotdata
#'
#' @param method 'plotsdatabase'
#' @param plot_name "mbalmayo001"
#'
#' @export
#'
#' @importFrom plotsdatabase query_plots
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise

import_plotdata <- function(method,plot_name){
   if(method == 'plotsdatabase') {
      Fieldplot_BDD_full = plotsdatabase::query_plots(plot_name = plot_name, show_all_coordinates = TRUE, map = F, extract_individuals = T)
      Tree_list <- Fieldplot_BDD_full$extract %>% dplyr::group_by(sous_plot_name) %>% dplyr::summarise (id = list(ind_num_sous_plot))
   }

   if(method == 'local_file') {
      print("PLEASE IMPORT INVENTORY DATA BY OUR OWN, THE DATA MUST BE NAMED 'Tree_list' ")
   }

   if(method == 'CAS1') {
      print("NO DATA AVAILABLE, JUST CONTINUE")
   }

   return(Fieldplot_BDD_full = Fieldplot_BDD_full)
} # Import the data from the database or by your own (password : Amap2020, username : ploton)
