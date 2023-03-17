
source("b5_simplified_MM_main.R")

### grid of (p,n,q,k,r)
plot_grid_pnqkr = expand.grid(
  n = 10^(0:8),
  k = 10^(0:8),
  # p = seq(0.5, 1, by=0.1),
  # q = seq(0,   1, by=0.1),
  # r = seq(0.5, 1, by=0.1)
  p = seq(0.5, 1, by=0.05),
  q = seq(0,   1, by=0.05),
  r = seq(0.5, 1, by=0.05)
) %>% 
  ## filter(n <= k) %>%
  # filter(r >= p) %>%
  arrange(p,n,q,k,r)
as_tibble(plot_grid_pnqkr)

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
FOLD = as.numeric(args[1])
folds <- createFolds(1:nrow(plot_grid_pnqkr), k = NUM_FOLDS_WPDF_PARALLELIZATION, list = TRUE, returnTrain = FALSE)
GRID = plot_grid_pnqkr[folds[[FOLD]], ]
  
##### tibble of win probability as a function of (p,n,q,k,r)  ##### 
##### for n q-chalky brackets vs. k r-chalky brackets         #####
result = numeric(nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0("i=",i, " of ", nrow(GRID)))
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
print("done")


