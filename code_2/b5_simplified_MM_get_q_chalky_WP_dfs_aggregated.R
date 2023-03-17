
source("b5_simplified_MM_main.R")

df_WP_nq_vs_kr = tibble()
for (FOLD in 1:NUM_FOLDS_WPDF_PARALLELIZATION) {
  filename = paste0("plot_thm1/df_WP_nq_vs_kr_fold",FOLD,".csv")
  df_WP_nq_vs_kr = bind_rows(df_WP_nq_vs_kr, read_csv(filename))
}
write_csv(df_WP_nq_vs_kr, paste0("df_WP_nq_vs_kr.csv"))



