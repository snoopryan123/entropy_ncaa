
source("a2_pick_6_main.R")

args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
fold_ = as.numeric(args[2])
num_folds_ = as.numeric(args[3])

# ### for testing
# version_ = 1
# fold_ = 35
# num_folds_ = 45

GRID1 = expand.grid(
  lambda = seq(0,1,by=0.05),
  index_a = 1:nrow(P),
  lambda_opp = 0.8
  # lambda_opp = c(0.75, 0.8, 0.85, 0.9)
) %>% as_tibble()
GRID1

# GRID1 = expand.grid(
#   lambda = seq(0,1,by=0.05),
#   a = 1:6,
#   lambda_opp = seq(0.5,1,by=0.05),
#   a_opp = 3
# ) %>% as_tibble()
# GRID1

### hyperparams
ks = sort(unique(c(10^(1:6), 5000, 15000, 25000, 50000, 150000, 250000)))
ns = ks
# ns = 10^(1:6)
# Cs = c(0,2500,38016,1e5,1e6)
Cs = c(0,38016,1e5)
alphas = c(0, 0.05, 0.18)

###############################################
### Expected Profit for Pick 6 Horse Racing ###
###############################################

print(paste0("version_=",version_))
if (version_ == 1) {
  GRID = GRID1
  GRID
} else {
  stop("this version_ has not yet been implemented")
}

### parallelize!
df_folds_0 =
  tibble(fold = cut(1:nrow(GRID), breaks=num_folds_,labels=FALSE)) %>%
  mutate(i = row_number()) 
df_folds = df_folds_0 %>% filter(fold == fold_)
idxs = df_folds$i
print(paste0("idxs ", idxs))
GRID = GRID[idxs,]
GRID$i = 1:nrow(GRID)
GRID

################################################################################

set.seed(1235124+fold_*23421) ### use fold_ to get a different random sample
results = list()
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  
  Q_i = lambda_chalky_P(lambda = GRID$lambda[i], index_a = GRID$index_a[i], P)
  R_i = naively_chalky_P(lambda_opp = GRID$lambda_opp[i], P)
  EP_i = E_profit(P,Q_i,R_i,ns,ks,Cs,alphas,print_every_n=1e5)
  EP_i$i = i
  results[[i]] = EP_i
}
results1 = bind_rows(results)
results2 = results1 %>% left_join(GRID) %>% select(-i)
write_csv(results2, paste0(output_folder,"plot_grid_eProfit_v",version_,"_fold",fold_,".csv"))




