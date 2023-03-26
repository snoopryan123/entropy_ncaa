
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

args = commandArgs(trailingOnly=TRUE)
GRID_ROW_IDX = as.numeric(args[1])
PARAMS = GRID[GRID_ROW_IDX,]

if (max(PARAMS$n, PARAMS$k) <= 10000) {
  NUM_RUNS = 150
} else {
  NUM_RUNS = 5 #FIXME
}

# hU_stars = numeric(NUM_RUNS)
hU_stars_espn = numeric(NUM_RUNS)
hU_stars_num_correct = numeric(NUM_RUNS)
# hL_stars = numeric(NUM_RUNS)
for (RUN in 1:NUM_RUNS) {
  ### true_backets
  set.seed(423942347+RUN*2) 
  true_backets = sample_n_brackets(n=250)
  
  opp_submitted_backets = sample_n_brackets(n=PARAMS$k, prob_method = PARAMS$opp_prob_method)
  opp_scores_espn = compute_max_score(opp_submitted_backets, true_backets, "ESPN")
  opp_scores_num_correct = compute_max_score(opp_submitted_backets, true_backets, "num_correct")
  
  ### find best hU
  GRID_hU = 38:58
  #### scores_hU = numeric(length(GRID_hU))
  # wp_hU = numeric(length(GRID_hU))
  # names(wp_hU) = GRID_hU
  wp_hU_espn = numeric(length(GRID_hU))
  names(wp_hU_espn) = GRID_hU
  wp_hU_num_correct = wp_hU_espn
  for (i in 1:length(GRID_hU)) {
    hU = GRID_hU[i]
    print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", length(GRID_hU), ", and hU = ", hU))
    our_submitted_backets = sample_n_brackets_entropyRange(PARAMS$n, -Inf, hU, prob_method="P_538_2022") 
    scores_espn = compute_max_score(our_submitted_backets, true_backets, "ESPN")
    scores_num_correct = compute_max_score(our_submitted_backets, true_backets, "num_correct")
    wp_hU_espn[i] = mean(scores_espn >= opp_scores_espn)
    wp_hU_num_correct[i] = mean(scores_num_correct >= opp_scores_num_correct)
  }
  hU_star_espn = GRID_hU[first(which(wp_hU_espn == max(wp_hU_espn)))]
  hU_star_num_correct = GRID_hU[first(which(wp_hU_num_correct == max(wp_hU_num_correct)))]
  print(hU_star_espn); print(hU_star_num_correct);
  hU_stars_espn[RUN] = hU_star_espn
  hU_stars_num_correct[RUN] = hU_star_num_correct
  
  # ### given hU, find best hL
  # hdiff = 2
  # GRID_hL = c(-Inf, 38:(hU_star - hdiff))
  # if (hU_star - hdiff > 38) {
  #   wp_hL = numeric(length(GRID_hL))
  #   names(wp_hL) = GRID_hL
  #   for (i in 1:length(GRID_hL)) {
  #     hL = GRID_hL[i]
  #     print(paste0("run = ", RUN, " of ", NUM_RUNS, ", i = ", i, " of ", length(GRID_hL), ", and hL = ", hL))
  #     our_submitted_backets = sample_n_brackets_entropyRange(PARAMS$n, hL, hU_star, prob_method="P_538_2022") 
  #     #### scores_hL[i] = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
  #     our_scores = compute_max_score(our_submitted_backets, true_backets, scoring_method=PARAMS$scoring_method)
  #     wp_hL[i] = mean(our_scores >= opp_scores)
  #   }
  #   hL_star = GRID_hL[last(which(wp_hL == max(wp_hL)))]
  # } else {
  #   hL_star = -Inf
  # }
  # wp_hL
  # 
  # print(c(hL_star,hU_star))
  
  # hL_stars[RUN] = hL_star
  # hU_stars[RUN] = hU_star
}

######
h_star = 
  tibble(hU_stars_espn, hU_stars_num_correct) %>%
  summarise(
    hU_star_espn = mean(hU_stars_espn),
    hU_star_num_correct = mean(hU_stars_num_correct)
  )
h_star
write_csv(h_star, paste0("dfs/df_h_star_i",GRID_ROW_IDX,".csv"))


# ######
# h_star = hU_stars %>% summarise(hU_star = mean(hU_stars))
# h_star
# write_csv(h_star, paste0("dfs/df_h_star_i",GRID_ROW_IDX,".csv"))

# ######
# h_stars = tibble(hL_stars, hU_stars)
# h_stars
#  
# h_star = h_stars %>%
#   mutate(hL_stars = ifelse(hL_stars == -Inf, 30, hL_stars)) %>%
#   summarise(hL_star = mean(hL_stars),
#             hU_star = mean(hU_stars))
# h_star
# write_csv(h_star, paste0("dfs/df_h_star_i",GRID_ROW_IDX,".csv"))





# min(compute_entropies(sample_n_brackets(n=1e5,keep_probs = T)))


