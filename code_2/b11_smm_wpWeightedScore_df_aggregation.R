
source("b11_smm_main.R")

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])

#FIXME
if (version_ == 1) {
  num_folds_parralelization_ = 47
} else if (version_ == 2) {
  num_folds_parralelization_ = 55
} else {
  stop("")
}

GRID = tibble()
for (fold_ in 1:num_folds_parralelization_) {
  df_fold = read_csv(paste0(output_folder,"plot_grid_wpScore_v",version_,"_fold",fold_,".csv"))
  GRID = bind_rows(GRID, df_fold)
}
write_csv(GRID, paste0(output_folder,"plot_grid_wpScore_v",version_,".csv"))



