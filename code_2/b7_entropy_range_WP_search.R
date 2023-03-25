
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

### true_backets
set.seed(99) # Aaron Donald
true_backets = sample_n_brackets(n=250)

args = commandArgs(trailingOnly=TRUE)
GRID_ROW_IDX = as.numeric(args[1])
PARAMS = GRID[GRID_ROW_IDX,]

if (min(PARAMS$n, PARAMS$k) <= 100) {
  NUM_RUNS = 25
} else {
  NUM_RUNS = 1 #FIXME
}

hU_stars = numeric(NUM_RUNS)
hL_stars = numeric(NUM_RUNS)
for (RUN in 1:NUM_RUNS) {
  opp_submitted_backets = sample_n_brackets(n=PARAMS$k, prob_method = PARAMS$opp_prob_method)
  opp_scores = compute_max_score(opp_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
  
  ### find best hU
  GRID_hU = 38:58
  #### scores_hU = numeric(length(GRID_hU))
  wp_hU = numeric(length(GRID_hU))
  names(wp_hU) = GRID_hU
  for (i in 1:length(GRID_hU)) {
    hU = GRID_hU[i]
    print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", length(GRID_hU), ", and hU = ", hU))
    our_submitted_backets = sample_n_brackets_entropyRange(PARAMS$n, -Inf, hU, prob_method="P_538_2022") 
    #### scores_hU[i] = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
    our_scores = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
    wp_hU[i] = mean(our_scores >= opp_scores)
  }
  wp_hU
  hU_star = GRID_hU[last(which(wp_hU == max(wp_hU)))]
  
  ### given hU, find best hL
  hdiff = 3
  GRID_hL = c(-Inf, 38:(hU_star - hdiff))
  if (hU - hdiff > 38) {
    wp_hL = numeric(length(GRID_hL))
    names(wp_hL) = GRID_hL
    for (i in 1:length(GRID_hL)) {
      hL = GRID_hL[i]
      print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", length(GRID_hL), ", and hL = ", hL))
      our_submitted_backets = sample_n_brackets_entropyRange(PARAMS$n, hL, hU_star, prob_method="P_538_2022") 
      #### scores_hL[i] = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
      our_scores = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
      wp_hL[i] = mean(our_scores >= opp_scores)
    }
    hL_star = GRID_hL[last(which(wp_hL == max(wp_hL)))]
  } else {
    hL_star = -Inf
  }
  wp_hL
  
  c(hL_star,hU_star)
  
  hL_stars[RUN] = hL_star
  hU_stars[RUN] = hU_star
}

######
h_stars = tibble(hL_stars, hU_stars)
h_stars
 
h_star = h_stars %>%
  mutate(hL_stars = ifelse(hL_stars == -Inf, 30, hL_stars)) %>%
  summarise(hL_star = mean(hL_stars),
            hU_star = mean(hU_stars))
h_star
write_csv(h_star, paste0("dfs/df_h_star_i",GRID_ROW_IDX,".csv"))





# min(compute_entropies(sample_n_brackets(n=1e5,keep_probs = T)))


