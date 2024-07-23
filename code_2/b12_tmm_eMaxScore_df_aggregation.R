
source("b12_tmm_main.R")

# version_ = 1
args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
if (version_ == 1 | version_1 == 2 | version_ == 3) {
  num_folds = 10 #FIXME
}

dfv = tibble()
for (fold_ in 1:num_folds) {
  df_fold = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v",version_,"_fold",fold_,".csv"))
  df_fold = df_fold %>% mutate(fold = fold_)
  dfv = bind_rows(dfv, df_fold)
}
# dfv = dfv %>% group_by()
write_csv(dfv, paste0(output_folder,"plot_grid_eMaxScore_v",version_,".csv"))
