library(tidyverse)
# library(purrr)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
SAVE_PLOT = TRUE #TRUE #FALSE

################
### Get Data ###
################

D = read_csv("../data/538_ELO.csv") %>% rename(game_id = initial_game_num) ##%>% arrange(game_id)

P_file = "P_538_2022" # "P_538_2022" # "P1"
plot_folder = paste0("plot_", P_file, "/")
P_ = read.csv( paste0("../data/", P_file, ".csv"), row.names = 1, header= TRUE)

P <- function(x) {
  # x = c(i,j)    ### return the probability team idx i beats team idx j
  i = x[1]; j = x[2];
  P_[i,j]
}

###########################
### Simulate N Brackets ###
###########################

reformat_simulated_brackets <- function(brackets) {
  left_join(brackets, D %>% select(team_idx, seed)) %>% 
    relocate(seed, .after=round) %>%
    arrange(bracket_idx,round,game_id) 
}

start_of_tourney = D %>% arrange(game_id) %>% select(team_idx, game_id) %>% mutate(round = 1)

sample_brackets_h_range <- function(N, hL=-Inf, hU=Inf) {
  ### sample N brackets x with hL <= H(x) <= hU
  N_sample_at_a_time = 10^4 ##round(log(N, base=10))
  result = tibble()
  num_sampled = 0
  while (num_sampled < N) {
    print(num_sampled)
    S = simulate_brackets(N_sample_at_a_time)
    keep = get_entropy_df(S) %>% filter(hL <= log_prob_x & log_prob_x <= hU)
    if ( min(c(N - num_sampled < nrow(keep))) ) {
      keep = keep[1:(N - num_sampled),]
    }
    S = S %>% filter(bracket_idx %in% keep$bracket_idx)
    result = bind_rows(result, S)
    num_sampled = num_sampled + nrow(keep)
  }
  result = result %>% mutate(bracket_idx = 1+floor((row_number()-1)/ 63) )
  result
}

get_S_scores <- function(submitted_bx, true_bx) {
  # tibble of submitted brackets `submitted_bx`
  # tibble of true brackets `true_bx`
  # make sure the brackets in each set have the same size
  S12 = tibble(submitted_bx)
  S12$true_team_idx = true_bx$team_idx
  S12 = S12 %>% mutate(
    correct_pick = team_idx - true_team_idx == 0,
    f1 = correct_pick*1,
    f2 = correct_pick*2^(round-2)*10,
    # f3 = correct_pick*( 2^(round-2) + seed)
  )
  S12 = S12 %>% group_by(bracket_idx) %>% summarise(f1 = sum(f1), f2 = sum(f2))
  S12
}


### in this file, reformat_simulated_brackets is called within this function!!
simulate_brackets <- function(N) {
  start_of_round = start_of_tourney
  start_of_round = do.call("rbind", replicate(N, start_of_round, simplify = FALSE)) %>% mutate(bracket_idx = 1+floor((row_number()-1)/64) )
  round = 1
  ##all_rounds = list(start_of_round)
  all_rounds = tibble() ##tibble(start_of_round)
  
  while (round <= 6) {
    team1s = start_of_round %>% filter(row_number() %% 2 == 1)
    team2s = start_of_round %>% filter(row_number() %% 2 == 0)
    matchups = cbind(team1s$team_idx, team2s$team_idx)
    # matchups = t(apply(matchups,1,sort))
    matchup_probs = apply(matchups, MARGIN=1, FUN=P)
    matchup_results = rbinom(n = length(matchup_probs), size = 1, prob = matchup_probs)
    ### log probability of this round occuring
    resulting_game_probs = matchup_results*matchup_probs + (1-matchup_results)*(1-matchup_probs)
    # log_probs = log(resulting_game_probs)
    ### winning teams of this round
    matchup_results_ = cbind(1:length(matchup_results), 2 - matchup_results)
    winners = matchups[matchup_results_]
    ### expected value of the Score of this round
    
    ### set up the next round
    round = round+1
    if (round <= 6) {
      aa = (1:(length(winners)/2) - 1) %% 2^(6-round) + 1
      game_id = c(rbind(aa,aa))
    } else {
      game_id = 1
    }
    start_of_round = tibble(team_idx = winners, game_id = game_id, 
                            prob = resulting_game_probs, round=round) %>%
      mutate(bracket_idx = 1+floor((row_number()-1)/ (2^(7-round)) ) )
    
    all_rounds = bind_rows(all_rounds, start_of_round)
  }
  reformat_simulated_brackets(all_rounds)
}

### true brackets 
num_t = 100 #100 #200 #1000
true_brackets = simulate_brackets(num_t)


######################
### Plot Functions ###
######################

get_max_scores <- function(brackets_set, print_b=TRUE) {
  max_scores = tibble()
  max_H1s = tibble()
  max_H2s = tibble()
  for (b in 1:num_t) {
    if (print_b) {print(b)}
    true_b = true_brackets %>% filter(bracket_idx == b)# & round > 1)
    # true_b_teams = true_b$team_idx
    brackets1_b = tibble(brackets_set) #tibble(brackets_set)
    ll = nrow(brackets_set) / nrow(true_b)
    brackets1_b$true_b_team = rep(true_b$team_idx, times=ll)
    # brackets1_b = brackets1_b %>% mutate(rid = row_number()) %>%
    #               left_join(tibble(true_b_team = rep(true_b$team_idx, times=ll)) %>% mutate(rid = row_number()),
    #                                by="rid")
    brackets1_b = brackets1_b %>% mutate(true_b_team = rep(true_b$team_idx, times=ll) )
    brackets1_b = brackets1_b %>% mutate(
      correct_pick = team_idx - true_b_team == 0,
      f1 = correct_pick*1,
      f2 = correct_pick*2^(round-2)*10,
      # f3 = correct_pick*( 2^(round-2) + seed)
    )
    scores_b = brackets1_b %>% group_by(bracket_idx) %>% 
      summarise(f1 = sum(f1), f2 = sum(f2), log_prob_x = -sum(log(prob,base=2)) )#, f3 = sum(f3))
    max_scores_b = scores_b %>% summarise(b=b, f1 = max(f1), f2 = max(f2))#, f3 = max(f3))
    max_scores = bind_rows(max_scores, max_scores_b)
    
    max_H1_b = scores_b %>% summarise(b=b, H1 = log_prob_x[f1==max(f1)])
    max_H2_b = scores_b %>% summarise(b=b, H2 = log_prob_x[f2==max(f2)])
    max_H1s = bind_rows(max_H1s, max_H1_b)
    max_H2s = bind_rows(max_H2s, max_H2_b)
  }
  list(max_scores, max_H1s, max_H2s)
}

get_entropy_df <- function(brackets_set) {
  brackets_set %>% group_by(bracket_idx) %>% summarise(log_prob_x = -sum(log(prob,base=2) )) 
}

###############################################

plot_max_scores_hist <- function(max_scores, title="", score=1) {
  pp = max_scores %>% ggplot() + labs(title=title)
  if (score == 1) {
    mm = mean(max_scores$f1)
    pp = pp + xlab("maximum number of correct picks") +
      geom_histogram(aes(x=f1), fill="black") +
      scale_x_continuous(breaks=seq(1,70,by=2))
  } else if (score == 2) {
    mm = mean(max_scores$f2)
    pp = pp + xlab("maximum ESPN score") +
      geom_histogram(aes(x=f2), fill="black") +
      scale_x_continuous(breaks=seq(100,2000,by=200))
  } else if (score == 3) {
    mm = mean(max_scores$f3)
    pp = pp + xlab("maximum Julian score") +
      geom_histogram(aes(x=f3), fill="black") 
  }
  pp = pp + geom_vline(xintercept = mm, color="dodgerblue2") +
    scale_y_continuous(breaks=seq(0,num_t,by=10))
  pp
}

plot_scores <- function(max_scores, str, title="", savePlot=FALSE) {
  p1 = plot_max_scores_hist(max_scores, title=title)
  p2 = plot_max_scores_hist(max_scores, title=title, score=2)
  if (savePlot) {
    ggsave(paste0(plot_folder, "p1_",str,".png"), p1, width=7,height=6)
    ggsave(paste0(plot_folder, "p2_",str,".png"), p2, width=7,height=6)
  }
  p1
}

plot_entropy_hist <- function(entropy_df, str, title="",savePlot=TRUE) {
  entropy_hist = entropy_df %>% ggplot() + 
    geom_histogram(aes(x=log_prob_x, y=..density..), fill="black",bins=100) + 
    geom_vline(aes(xintercept = mean(log_prob_x)), color="dodgerblue2") +
    theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("H") +
    labs(title=title)
  entropy_hist
  if (savePlot) {
    ggsave(paste0(plot_folder, "entropy_hist_",str,".png"), entropy_hist, width=7,height=6)
  }
  entropy_hist
}





