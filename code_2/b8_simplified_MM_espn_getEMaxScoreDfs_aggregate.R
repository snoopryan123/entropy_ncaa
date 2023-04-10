
source("b8_simplified_MM_espn_main.R")

NUM_FOLDS_WPDF_PARALLELIZATION = 50 #FIXME
df_eMaxEspnScoreSmm = tibble()
for (version_ in 1:2) {
  for (FOLD in 1:NUM_FOLDS_WPDF_PARALLELIZATION) {
    filename = paste0("dfs/df_eMaxEspnScore_SMM_f",FOLD,"v",version_,".csv")
    df_eMaxEspnScoreSmm = bind_rows(df_eMaxEspnScoreSmm, read_csv(filename))
  }
  write_csv(df_eMaxEspnScoreSmm, paste0("df_eMaxEspnScoreSmm_v",version_,".csv"))
}
print("done")




