
source("b5_simplified_MM_main.R")

### for some reason, expand.grid and tidyverse::crossing WEREN'T WORKING
### in that they were MISSING SOME COMBINATIONS of (n,k,p,q,r)...
### this was driving me crazy
### so I instead mdae the combos in PYTHON and import them here...
plot_grid_pnqkr = read_csv("plot_grid_pnqkr.csv") %>% select(-`...1`)
plot_grid_pnqkr
plot_grid_pnqkr = plot_grid_pnqkr %>% filter(p >= 0.5 & r >= 0.5)
plot_grid_pnqkr

# ### checks
# plot_grid_pnqkr %>% filter(p == 0.9)
# plot_grid_pnqkr %>% filter(p == 0.95)
# plot_grid_pnqkr %>% filter(r == 0.95)
# plot_grid_pnqkr %>% filter(p == 0.95) %>% filter(r == 0.95)
# unique(plot_grid_pnqkr$q)
# plot_grid_pnqkr %>% filter(q == 0.95)
# plot_grid_pnqkr[plot_grid_pnqkr$q==.95,]
# plot_grid_pnqkr[plot_grid_pnqkr$q==.05,]
# plot_grid_pnqkr %>% filter(p == 0.95 & r == 0.95 & q == 0.95)

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
FOLD = as.numeric(args[1])
folds <- caret::createFolds(1:nrow(plot_grid_pnqkr), k = NUM_FOLDS_WPDF_PARALLELIZATION, list = TRUE, returnTrain = FALSE)
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


