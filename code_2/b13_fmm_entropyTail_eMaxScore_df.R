
source("a2_main.R")

output_folder = "./plot_13/"

entropy_tailMap_filename = paste0(output_folder, "df_fmm_entropy_tail_map.csv")
if (file.exists(entropy_tailMap_filename)) {
  entropy_tailMap = read_csv(entropy_tailMap_filename)
} else {
  stop("first, run the file `b13_fmm_entropyTail_map.R` to get `df_fmm_entropy_tail_map.csv` ")
}

############################
### Grids to Search Over ###
############################

### grid to search over
GRID1 = expand.grid(
  h = sort(unique(entropy_tailMap$h)),
  # n = 10^(0:3)
  n = 10^(0:4)
  # n = 10^(1:4)
)
GRID1

######################################################################
### To maximize your expected score of in the bracket challenge,   ###
### given the "true" known win probabilities P,                    ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

# version_ = 1
# fold_ = 1
# num_folds_ = 1

args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1]) ### which GRID to search over
fold_ = as.numeric(args[2])
num_folds_ = as.numeric(args[3])

### get GRID
if (version_ == 1) {
  GRID_OG = GRID1
} else {
  stop("this `version_` has not yet been implemented")
}

### map h in GRID_OG to (left or right tail, entropy cutoff)
entropy_tailMap_1 = entropy_tailMap %>% rename(entropy_cutoff = h, mean_entropy = H) #%>% mutate(H_rounded = round(H))
GRID_OG_A = GRID_OG %>% 
  fuzzyjoin::difference_left_join(entropy_tailMap_1, max_dist = Inf, 
                                  distance_col = "dist", by=c("h" = "mean_entropy")) %>%
  group_by(n,h) %>%
  arrange(n,h,dist) %>%
  slice_head() %>%
  ungroup() %>% select(-c(mean_entropy, dist))
GRID_OG_A

### get GRID from this fold
if (num_folds_ > 1) {
  df_folds_0 = tibble(fold = cut(1:nrow(GRID_OG_A), breaks=num_folds_, labels=FALSE)) %>% mutate(i = row_number()) 
} else {
  df_folds_0 = tibble(fold = 1, i = 1:nrow(GRID_OG_A))
}
df_folds = df_folds_0 %>% filter(fold == fold_)
idxs = df_folds$i
print(paste0("idxs ", idxs))
GRID = GRID_OG_A[idxs,]
GRID

#### keep the runtime down...
if (max(GRID$n) <= 10000) {
  NUM_RUNS = 100 #500
} else {
  NUM_RUNS = 5 #FIXME
}

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
    hL = if (GRID$left_tail[i]) -Inf else GRID$entropy_cutoff[i]
    hU = if (GRID$left_tail[i]) GRID$entropy_cutoff[i] else Inf
    our_submitted_backets = sample_n_brackets_entropyRange(n, hL, hU, prob_method="P_538_2022") 
    
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
write_csv(GRID, paste0(output_folder, "plot_grid_eMaxScore_fmm_v",version_,"_fold_",fold_,".csv"))


