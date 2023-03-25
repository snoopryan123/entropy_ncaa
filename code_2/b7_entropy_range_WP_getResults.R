
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

GRID_results = tibble()
idxs_not_successful = numeric()
for (GRID_ROW_IDX in 1:nrow(GRID)) {
  filename = paste0("dfs/df_h_star_i",GRID_ROW_IDX,".csv")
  if (file.exists(filename)) {
    dfi = read_csv(filename)
    GRID_results = bind_rows(GRID_results, dfi)
  } else {
    idxs_not_successful = c(idxs_not_successful, GRID_ROW_IDX)
  }
}

write_csv(GRID_results, "df_entropy_range_grid_results.csv")
print("idxs_not_successful")
print(idxs_not_successful)


