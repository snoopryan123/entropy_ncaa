library(tidyverse)
library(latex2exp)
# library(purrr)
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))
SAVE_PLOT = TRUE #TRUE #FALSE

################
### Get Data ###
################

D = read_csv("../data/538_ELO.csv") %>% rename(game_id = initial_game_num) ##%>% arrange(game_id)
START_OF_TOURNEY = D %>% arrange(game_id) %>% select(team_idx, game_id, seed) %>% mutate(round = 1)
START_OF_TOURNEY

P_538_2022 = read.csv( paste0("../data/P_538_2022.csv"), row.names = 1, header= TRUE)

P <- function(team_1s, team_2s, prob_method="P_538_2022") {
  if (prob_method == "P_538_2022") {
    ### team i beats team j according to ELO
    probs = P_538_2022[ cbind(team_1s, team_2s) ]
  } else if (prob_method == "naive_chalky") {
    ### team i beats team j w.p. 0.90 if better seed
    seed_1s = (tibble(team_idx = team_1s) %>% left_join(START_OF_TOURNEY, by = "team_idx"))$seed
    seed_2s = (tibble(team_idx = team_2s) %>% left_join(START_OF_TOURNEY, by = "team_idx"))$seed
    probs = (seed_1s - seed_2s < 0)*0.9 + (seed_1s - seed_2s == 0)*0.5 + (seed_1s - seed_2s > 0)*0.1
  } else {
    stop("prob_method not implemented in P function")
  }
  return(probs)
}

###
P_file = "P_538_2022" # "P_538_2022" # "P1"
plot_folder = paste0("plot_", P_file, "/")
##output_folder = paste0("job_outpit",P_file, "/")

###########################
### Simulate N Brackets ###
###########################

### helper functions
odd <- function(x) x%%2 != 0
even <- function(x) x%%2 == 0
sample_winners <- function(probs) { as.numeric( runif(n = length(probs)) <= probs ) }

### sample N brackets
sample_n_brackets <- function(n, prob_method="P_538_2022") {
  #####
  # n == number of brackets to be sampled
  # prob_method in {"P_538_2022", "naively_chalky"} 
  # determines the probability of team i beating team j
  #####
  tourney_n = list()
  tourney_n$n = n
  tourney_n$rd0_n = rep(START_OF_TOURNEY$team_idx, n)
  
  for (rd in 1:6) {
    curr_rd = tourney_n[[paste0("rd",rd-1,"_n")]]
    
    team_1s = curr_rd[odd(1:length(curr_rd))]
    team_2s = curr_rd[even(1:length(curr_rd))]
    probs = P(team_1s, team_2s, prob_method=prob_method)
    winners = sample_winners(probs)
    
    next_rd = team_1s*winners + team_2s*(1-winners)
    tourney_n[[paste0("rd",rd,"_n")]] = next_rd
  }
  
  tourney_n$rd0_n <- NULL
  
  return(tourney_n)
}

# bracket_set_a = sample_n_brackets(n=5)
# bracket_set_d = sample_n_brackets(n=5, prob_method = "naive_chalky")
# bracket_set_b = sample_n_brackets(n=1e4)
# bracket_set_c = sample_n_brackets(n=1e5)

###########################
###  Score bracket sets ###
###########################

compute_max_score <- function(submitted_brackets, true_brackets, scoring_method="ESPN", expected_score=F) {
  #####
  # submitted_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS submitted brackets
  # true_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS true brackets, for MoNUM_TRUE_BRACKETSe Carlo simulation
  # scoring_method in {"ESPN", "num_correct"} determines
  # the score f(x|tau) of a submitted bracket x relative to a true bracket tau
  #####
  NUM_TRUE_BRACKETS = true_brackets$n
  NUM_SUBD_BRACKETS = submitted_brackets$n
  scores_mat = matrix(nrow=NUM_TRUE_BRACKETS, ncol=NUM_SUBD_BRACKETS)
  
  for (tb in 1:NUM_TRUE_BRACKETS) {
    scores_tb = numeric(NUM_SUBD_BRACKETS)
    
    for (rd in 1:6) {
      rd_size = 2^(6-rd)
      true_rd = true_brackets[[paste0("rd",rd,"_n")]] [(1:rd_size)+(tb-1)*rd_size]
      true_rd = rep(true_rd, NUM_SUBD_BRACKETS)
      sub_rd = submitted_brackets[[paste0("rd",rd,"_n")]]
      
      if (scoring_method == "ESPN") {
        scores_rd = as.numeric(true_rd == sub_rd) * 2^(rd-1) * 10
      } else if (scoring_method == "num_correct") {
        scores_rd = as.numeric(true_rd == sub_rd)
      }
      
      scores_rd_mat = matrix(scores_rd, nrow = NUM_SUBD_BRACKETS, ncol = rd_size)
      scores_tb = scores_tb + rowSums(scores_rd_mat)
      
    }
    
    scores_mat[tb,] = scores_tb
  }
  row_maxes = apply(scores_mat, MARGIN=1, FUN=max)
  if (expected_score) {
    return( mean(row_maxes) )
  } else {
    return( row_maxes )
  }
}

# ### ex
# ex_true_backets = sample_n_brackets(n=100)
# 
# n = 100000
# ex_submitted_backets_elo = sample_n_brackets(n=n, prob_method = "P_538_2022")
# ex_submitted_backets_naiveChalky = sample_n_brackets(n=n, prob_method = "naive_chalky")
# ex_scores_elo = compute_max_score(ex_submitted_backets_elo, ex_true_backets, scoring_method="ESPN")
# ex_scores_naiveChalky = compute_max_score(ex_submitted_backets_naiveChalky, ex_true_backets, scoring_method="ESPN")
# ex_score_diffs = ex_scores_elo - ex_scores_naiveChalky
# ex_score_diffs = ex_score_diffs[ex_score_diffs != 0]
# mean(ex_score_diffs > 0)






