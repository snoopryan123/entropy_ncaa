
source("b8_simplified_MM_espn_main.R")

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
FOLD = as.numeric(args[1])
idxs_lower = nrow(plot_grid_maxEspnScore_SMM)/NUM_FOLDS_WPDF_PARALLELIZATION * (FOLD-1)
idxs_upper = nrow(plot_grid_maxEspnScore_SMM)/NUM_FOLDS_WPDF_PARALLELIZATION * FOLD
GRID = plot_grid_maxEspnScore_SMM[floor(idxs_lower):ceiling(idxs_upper),]

##### tibble of expected max score as a function of (p,n,q1,q2,...,q6)  ##### 
result = matrix(nrow=nrow(GRID), ncol=length(ns))
colnames(result) = paste0("eMaxEspnScore_n=",ns)
for (i in 1:nrow(GRID)) {
  print(paste0("Grid iter i = ",i, " of ", nrow(GRID)))
  print(GRID[i,])
  
  result_i = eMaxEspnScore_SMM(
     m,  
     p=GRID$p[i], 
     qrs=as.numeric(GRID[i,str_detect(colnames(GRID),"^q")]), 
     score="ESPN",
     print_num0=i, print_num1=nrow(GRID), print_every_n=10000
  ) ### takes ~5 minutes
  result[i,] = result_i
  gc() ### garbage collector, to free up memory for HPCC 
}
GRID = cbind(GRID, result)
filename = paste0("dfs/df_eMaxEspnScore_SMM_",FOLD,".csv")
write_csv(GRID, paste0(filename))
print("done")




