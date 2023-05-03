
source("b12_tmm_main.R")

num_folds = 10
for (version_ in 1:3) {
  dfv = tibble()
  for (fold_ in 1:num_folds) {
    df_fold = read_csv(paste0(output_folder,"plot_grid_wpScore_v",version_,"_fold",fold_,".csv"))
    df_fold = df_fold %>% mutate(fold = fold_)
    dfv = bind_rows(dfv, df_fold)
  }
  # dfv = dfv %>% group_by()
  write_csv(dfv, paste0(output_folder,"plot_grid_wpScore_v",version_,".csv"))
}


