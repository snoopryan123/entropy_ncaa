
source("b8_simplified_MM_espn_main.R")

### since this is so computationally intensive, split into folds
NUM_FOLDS_WPDF_PARALLELIZATION = 50 #FIXME
args = commandArgs(trailingOnly=TRUE)
FOLD = as.numeric(args[1])
version_ = as.numeric(args[2])
if (version_ == 1) {
  GRID = plot_grid_maxEspnScore_SMM_v1
} else if (version_ == 2) {
  GRID = plot_grid_maxEspnScore_SMM_v2
} else {
  stop("this version_ has not yet been implemented")
}
idxs_lower = nrow(GRID)/NUM_FOLDS_WPDF_PARALLELIZATION * (FOLD-1)
idxs_upper = nrow(GRID)/NUM_FOLDS_WPDF_PARALLELIZATION * FOLD
GRID = GRID[floor(idxs_lower):ceiling(idxs_upper),]

##### tibble of expected max score as a function of (p,n,q1,q2,...,q6)  ##### 
result = matrix(nrow=nrow(GRID), ncol=length(ns))
colnames(result) = paste0("eMaxEspnScore_n=",ns)
for (i in 1:nrow(GRID)) {
  print(paste0("Grid iter i = ",i, " of ", nrow(GRID)))
  print(GRID[i,])
  
  result[i,] = eMaxEspnScore_SMM(
     m,  
     p=GRID$p[i], 
     qrs=as.numeric(GRID[i,str_detect(colnames(GRID),"^q")]), 
     score=GRID$score[i],
     print_num0=i, print_num1=nrow(GRID), print_every_n=10000
  ) ### takes ~5 minutes
}
GRID = cbind(GRID, result)
filename = paste0("dfs/df_eMaxEspnScore_SMM_f",FOLD,"v",version_,".csv")
write_csv(GRID, paste0(filename))
print("done")




