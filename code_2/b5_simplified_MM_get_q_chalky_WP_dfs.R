
source("b5_simplified_MM_main.R")

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
FOLD = as.numeric(args[1])
NUM_FOLDS = 50 #FIXME
folds <- createFolds(1:nrow(plot_grid_pnqkr), k = NUM_FOLDS, list = TRUE, returnTrain = FALSE)
GRID = plot_grid_pnqkr[folds[[FOLD]], ]
  
##### tibble of win probability as a function of (p,n,q,k,r)  ##### 
##### for n q-chalky brackets vs. k r-chalky brackets         #####
result = numeric(nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0("i=",i))
  print(GRID[i,])
  
  n = GRID[i,]$n
  k = GRID[i,]$k
  p = GRID[i,]$p
  q = GRID[i,]$q
  r = GRID[i,]$r
  
  result[i] = WP_nq_vs_kr(m=m,p=p,n=n,q=q,k=k,r=r)
}
GRID$wp = result
filename = paste0("plot_thm1/df_WP_nq_vs_kr_fold",FOLD,".csv")
write_csv(GRID, paste0(filename))




# filename = paste0("plot_thm1/df_WP_nq_vs_kr.csv")


