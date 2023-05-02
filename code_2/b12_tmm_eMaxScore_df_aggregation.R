
source("b12_tmm_main.R")

# version_ = 1
args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
if (version_ == 1) {
  num_folds = 10 #FIXME
}

dfv = tibble()
for (fold_ in 1:num_folds) {
  df_fold = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v",version_,"_fold",fold_,".csv"))
  dfv = bind_rows(dfv, df_fold)
}
write_csv(dfv, paste0(output_folder,"plot_grid_eMaxScore_v",version_,".csv"))
