
# version_ = 1
# fold_ = 1
# num_folds_ = 19
# PROB_METHOD = "P1"

args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1]) ### which GRID to search over
fold_ = as.numeric(args[2])
num_folds_ = as.numeric(args[3])
PROB_METHOD = as.character(args[4])

###################
filewd = getwd()
setwd("..")
source("a2_main.R")
setwd(filewd)
###################

############################
### Grids to Search Over ###
############################

### grid to search over
GRID1 = expand.grid(
  # lambda = seq(0,1,by=0.05),
  # n = 10^(2:4),
  # strat = c(1,2),
  lambda = seq(0,1,by=0.025),
  n = sort(unique(c(seq(100,1000,by=100), seq(1000,10000,by=1000)))),
  strat = 3 #1
)
GRID1
nrow(GRID1)

###################
### Parallelize ###
###################

### get GRID
if (version_ == 1) {
  GRID_OG = GRID1
} else {
  stop("version_ = ", version_, " has not yet been implemented")
}

### get GRID from this fold
if (num_folds_ > 1) {
  df_folds_0 = tibble(fold = cut(1:nrow(GRID_OG), breaks=num_folds_, labels=FALSE)) %>% mutate(i = row_number()) 
} else {
  df_folds_0 = tibble(fold = 1, i = 1:nrow(GRID_OG))
}
df_folds = df_folds_0 %>% filter(fold == fold_)
idxs = df_folds$i
print(paste0("idxs ", idxs))
GRID = GRID_OG[idxs,]
GRID

#### keep the runtime down...
if (max(GRID$n) <= 10000) {
  NUM_RUNS = 100 #500
} else {
  NUM_RUNS = 5 #FIXME
}

######################################################################
### To maximize your expected score of in the bracket challenge,   ###
### given the "true" known win probabilities P,                    ###
### and given that you submit n brackets,                          ###
### how chalky should our submitted brackets be?                   ###
######################################################################

eMaxESPN_scores = matrix(nrow=NUM_RUNS, ncol=nrow(GRID))
rownames(eMaxESPN_scores) = paste0("RUN=",1:NUM_RUNS)
colnames(eMaxESPN_scores) = paste0("i=",1:nrow(GRID))
eMaxHamming_scores = eMaxESPN_scores
for (RUN in 1:NUM_RUNS) {
  set.seed(423942347+RUN*2) ### use RUN to get a different random sample
  
  true_backets = sample_n_brackets(n=250) ### true_backets
  
  ### find the expected max score from sampling n brackets from (-inf, hU] or [hL, -inf)
  for (i in 1:nrow(GRID)) {
    print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", nrow(GRID)))
    
    ### sample n brackets from the given entropy range
    n = GRID$n[i]
    lambda = GRID$lambda[i]
    strat = GRID$strat[i]
    our_submitted_backets = sample_n_brackets_chalkyLambda(n, lambda, strat, prob_method=PROB_METHOD) 
    
    ### compute the expected max score of our sampled bracket set
    scores_ESPN    = compute_max_score(our_submitted_backets, true_backets, "ESPN", expected_score=T)
    scores_Hamming = compute_max_score(our_submitted_backets, true_backets, "num_correct", expected_score=T)
    eMaxESPN_scores[RUN,i] = scores_ESPN            ## mean(scores_espn >= opp_scores_espn)
    eMaxHamming_scores[RUN,i] = scores_Hamming      ## mean(scores_num_correct >= opp_scores_num_correct)
  }
}

### add the results to GRID
GRID$eMaxESPN_scores = colMeans(eMaxESPN_scores)
GRID$eMaxHamming_scores = colMeans(eMaxHamming_scores)
print(GRID)

### save GRID
write_csv(GRID, paste0(output_folder, "plot_grid_eMaxScore_fmm_chalkyLambda_v",version_,"_fold_",fold_,".csv"))


