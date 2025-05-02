all_scans_report <- function (Fieldplot_BDD_full, all, directory, plot_name) {
   sink(file = paste0(directory, '/', plot_name,"/5_ALL_PLOT_REPORT.txt"))

   all_missing_id <- unique(Fieldplot_BDD_full$extract$ind_num_sous_plot)[!(unique(Fieldplot_BDD_full$extract$ind_num_sous_plot) %in% unique(all$id))] %>% as.numeric %>% sort()
   aa <- all %>% filter(where == 'in') %>% .[['id']] %>% unique()
   all_missing_id_in <- unique(Fieldplot_BDD_full$extract$ind_num_sous_plot)[!(unique(Fieldplot_BDD_full$extract$ind_num_sous_plot) %in% aa)] %>% as.numeric %>% sort()


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                       MISSING ID IN THE SCAN                       '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))


   print(paste('The following id are not present on the scans :', paste(all_missing_id_in, collapse = ', ')))


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('    MISSING ID IN THE SCAN AND THE SUBPLOT TO BE (RE)SCANNED        '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))


   print(Fieldplot_BDD_full$extract %>% filter(ind_num_sous_plot %in% all_missing_id_in) %>% group_by(sous_plot_name) %>%  summarise(id = paste(ind_num_sous_plot, collapse = ', ')))


   print(paste('                                                                    '))
   print(paste('        MISSING ID CAN BE DUE TO THE FOLLOWING MISTAKE              '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   adjacent_error <- all %>% filter(where == 'adjacent')
   out_error <- all %>% filter(where == 'out')

   if(nrow(adjacent_error) > 0) {

      for (i in 1:nrow(adjacent_error)) {

         print(paste('In the file :', adjacent_error$file[i], ', the id', adjacent_error$id[i], ' is present but it is suppose to be on the adjacent subplot', adjacent_error$sous_plot_name[i]))

      }
   }

   if(nrow(out_error) > 0) {

      for (i in 1:nrow(out_error)) {

         print(paste('In the file :', out_error$file[i], ', the id', out_error$id[i], ' is present but it is suppose to be on the another subplot which is not adjacent', out_error$sousplot[i]))

      }
   }


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   duplicate_id_same_file <- all %>%
      filter(duplicated_id == 'yes' & what == 'tree') %>%
      group_by(id) %>%
      summarise(file = unique(file),
                n = n())

   if (nrow(duplicate_id_same_file) > 0) {


      for (i in 1:nrow(duplicate_id_same_file)){
         print(paste('In the file :', duplicate_id_same_file$file[i], ', the id', duplicate_id_same_file$id[i], ' is present', duplicate_id_same_file$n[i], ' times'))

      }


   }

   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                FILES WITH LESS THAN 3 REFERENCES                   '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   less_than_3_ref <- all %>%
      group_by(file) %>%
      slice(1) %>%
      select(file, n_jalon_ref, n_tree_ref, n_tot_ref) %>%
      filter(n_tot_ref < 3)

   if (nrow(less_than_3_ref) > 0) {


      for (i in 1:nrow(less_than_3_ref)){
         print(paste('The file :', less_than_3_ref$file[i], 'has only', less_than_3_ref$n_tot_ref[i], ' reference(s)'))
         print(paste('It has :', less_than_3_ref$n_jalon_ref[i], ' jalon(s)'))
         print(paste('It has :', less_than_3_ref$n_tree_ref[i], ' tree(s)'))
         print(paste('                                                                    '))

      }


   }

   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                  FILES WITH LESS THAN 3 JALONS                     '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   less_than_3_jalon <- all %>%
      group_by(file) %>%
      slice(1) %>%
      select(file, n_jalon_ref, n_tree_ref, n_tot_ref) %>%
      filter(n_jalon_ref < 3)

   if (nrow(less_than_3_jalon) > 0) {


      for (i in 1:nrow(less_than_3_jalon)){
         print(paste('The file :', less_than_3_jalon$file[i], 'has only', less_than_3_jalon$n_jalon_ref[i], ' jalon(s) as reference(s)'))
         print(paste('It has :', less_than_3_jalon$n_jalon_ref[i], ' jalon(s)'))
         print(paste('It has :', less_than_3_jalon$n_tree_ref[i], ' tree(s)'))
         print(paste('                                                                    '))

      }


   }

   sink(file =NULL)

   print(paste('A report has been saved : ', paste0(directory, '/',plot_name,"/5_ALL_PLOT_REPORT.txt")))

} # ALL SCANS REPORT
