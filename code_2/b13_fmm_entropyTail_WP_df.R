
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
  h = sort(unique(entropy_tailMap$h)), ### how chalky we are
  opp_prob_method = c("naive_chalky"), ### how chalky they are
  n = 10^(2:4),
  k = 10^(2:4)
) %>%
  filter(n <= k) %>%
  # filter(n < k) %>% 
  arrange(n,k)
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
  group_by(n,k,h) %>%
  arrange(n,k,h,dist) %>%
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

wpESPN_scores = matrix(nrow=NUM_RUNS, ncol=nrow(GRID))
rownames(wpESPN_scores) = paste0("RUN=",1:NUM_RUNS)
colnames(wpESPN_scores) = paste0("i=",1:nrow(GRID))
wpHamming_scores = wpESPN_scores
for (RUN in 1:NUM_RUNS) {
  set.seed(423942347+RUN*2) ### use RUN to get a different random sample
  
  true_backets = sample_n_brackets(n=250) ### true_backets
  
  ### find the expected max score from sampling n brackets from (-inf, hU] or [hL, -inf)
  for (i in 1:nrow(GRID)) {
    print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", nrow(GRID)))
    
    ### sample our n brackets from the given entropy range
    n = GRID$n[i]
    hL = if (GRID$left_tail[i]) -Inf else GRID$entropy_cutoff[i]
    hU = if (GRID$left_tail[i]) GRID$entropy_cutoff[i] else Inf
    our_submitted_backets = sample_n_brackets_entropyRange(n, hL, hU, prob_method="P_538_2022") 
    
    ### sample opponents' k brackets
    k = GRID$k[i]
    opp_prob_method = as.character(GRID$opp_prob_method[i])
    if (opp_prob_method == "super_chalky") {
      opp_submitted_backets = sample_n_brackets_entropyRange(n=k, hL=-Inf, hU=45.75)
    } else {
      opp_submitted_backets = sample_n_brackets(n=k, prob_method = opp_prob_method)
    }
    
    ### compute the bracket scores
    our_scores_ESPN    = compute_max_score(our_submitted_backets, true_backets, "ESPN")
    our_scores_Hamming = compute_max_score(our_submitted_backets, true_backets, "num_correct")
    opp_scores_ESPN    = compute_max_score(opp_submitted_backets, true_backets, "ESPN")
    opp_scores_Hamming = compute_max_score(opp_submitted_backets, true_backets, "num_correct")
    
    ### compute the win proportions
    wpESPN_scores[RUN,i]    = mean(our_scores_ESPN >= opp_scores_ESPN)
    wpHamming_scores[RUN,i] = mean(our_scores_Hamming >= opp_scores_Hamming)
  }
}

### add the results to GRID
GRID$wpESPN_scores = colMeans(wpESPN_scores)
GRID$wpHamming_scores = colMeans(wpHamming_scores)
print(GRID)

### save GRID
write_csv(GRID, paste0(output_folder, "plot_grid_wp_fmm_v",version_,"_fold_",fold_,".csv"))


