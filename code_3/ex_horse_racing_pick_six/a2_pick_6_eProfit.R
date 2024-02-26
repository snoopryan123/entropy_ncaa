
source("a2_pick_6_main.R")

args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
fold_ = as.numeric(args[2])
num_folds_ = as.numeric(args[3])

# ### for testing
# version_ = 1
# fold_ = 35
# num_folds_ = 45

###################
### Parallelize ###
###################

GRID1 = expand.grid(
  lambda = c(0.5, 1, 1.25, 1.5, 2),
  a = (1:nrow(P))/nrow(P),
  lambda_opp = c(1/4, 1/2, 2/3, 4/5, 1, 5/4, 3/2, 2, 4)
) %>% as_tibble()
GRID1

### hyperparams
# ks = c(1e5, 5e5, 1e6, 1e7)
ks = 1e6
ns = c(1/100, 1/50, 1/25, seq(1/10, 1, by=1/10))*ks
Cs = c(0,38016, 1e6)
alphas = c(0.05)

print(paste0("version_=",version_))
if (version_ == 1) {
  GRID = GRID1
  GRID
} else {
  stop("this version_ has not yet been implemented")
}

### parallelize
df_folds_0 =
  tibble(fold = cut(1:nrow(GRID), breaks=num_folds_,labels=FALSE)) %>%
  mutate(i = row_number()) 
df_folds = df_folds_0 %>% filter(fold == fold_)
idxs = df_folds$i
print(paste0("idxs ", idxs))
GRID = GRID[idxs,]
GRID$i = 1:nrow(GRID)
GRID

###############################################
### Expected Profit for Pick 6 Horse Racing ###
###############################################

set.seed(1235124+fold_*23421) ### use fold_ to get a different random sample
results = list()
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  
  Q_i = tilted_P(P, lambda = GRID$lambda[i], a = GRID$a[i])
  R_i = tilted_P_divMax(P, lambda_opp = GRID$lambda_opp[i])
  EP_i = E_profit(P,Q_i,R_i,ns,ks,Cs,alphas,print_every_n=1e5)
  EP_i$i = i
  results[[i]] = EP_i
}
results1 = bind_rows(results)
results2 = results1 %>% left_join(GRID, multiple = "all") %>% select(-i)
write_csv(results2, paste0(output_folder,"plot_grid_eProfit_v",version_,"_fold",fold_,".csv"))




