source("2_main.R")

######################################
######### N Random Brackets ##########
######################################

r100_brackets = simulate_brackets(10^2)
r100_scores = get_max_scores(r100_brackets)
r100_ent = get_entropy_df(r100_brackets)
# plot_scores(r100_scores, "r100", title="100 randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(r100_ent, "r100", title="100 randomly sampled brackets",savePlot=SAVE_PLOT)

r1k_brackets = simulate_brackets(10^3)
r1k_scores = get_max_scores(r1k_brackets)
r1k_ent = get_entropy_df(r1k_brackets)
# plot_scores(r1k_scores, "r1k", title="1k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(r1k_ent, "r1k", title="1k randomly sampled brackets",savePlot=SAVE_PLOT)

r10k_brackets = simulate_brackets(10^4)
r10k_scores = get_max_scores(r10k_brackets)
r10k_ent = get_entropy_df(r10k_brackets)
# plot_scores(r10k_scores, "r10k", title="10k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(r10k_ent, "r10k", title="10k randomly sampled brackets",savePlot=SAVE_PLOT)

# H = mean(r10k_ent$log_prob_x)
# H
# nu = var(r10k_ent$log_prob_x)
# nu

r100k_brackets = simulate_brackets(10^5)
# r100k_scores = get_max_scores(r100k_brackets)
r100k_ent = get_entropy_df(r100k_brackets)
# plot_scores(r100k_scores, "r100k", title="100k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(r100k_ent, "r100k", title="100k randomly sampled brackets",savePlot=SAVE_PLOT)

##############################################
### Top K Brackets from N Random Brackets  ###
##############################################

get_top_K_brackets <- function(K, brackets_set) {
  brackets_set %>% 
    group_by(bracket_idx) %>%
    summarise( log_prob_x = -sum(log(prob,base=2)) ) %>%
    arrange(log_prob_x) %>%
    filter(row_number() <= K) # top 1000 brackets
}

t100_of_100k_brackets_by_ent = get_top_K_brackets(100, r100k_brackets)
t100_of_100k_brackets = r100k_brackets %>% filter(bracket_idx %in% t100_of_100k_brackets_by_ent$bracket_idx)
t100_of_100k_scores = get_max_scores(t100_of_100k_brackets)
t100_of_100k_ent = get_entropy_df(t100_of_100k_brackets) %>% arrange(log_prob_x)
# plot_scores(t100_of_100k_scores, "t100_of_100k", title="Top 100 of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(t100_of_100k_ent, "t100_of_100k", title="Top 100 of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

t1k_of_100k_brackets_by_ent = get_top_K_brackets(1000, r100k_brackets)
t1k_of_100k_brackets = r100k_brackets %>% filter(bracket_idx %in% t1k_of_100k_brackets_by_ent$bracket_idx)
t1k_of_100k_scores = get_max_scores(t1k_of_100k_brackets)
t1k_of_100k_ent = get_entropy_df(t1k_of_100k_brackets)
# plot_scores(t1k_of_100k_scores, "t1k_of_100k", title="Top 1K of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(t1k_of_100k_ent, "t1k_of_100k", title="Top 1K of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

t10k_of_100k_brackets_by_ent = get_top_K_brackets(10^4, r100k_brackets)
t10k_of_100k_brackets = r100k_brackets %>% filter(bracket_idx %in% t10k_of_100k_brackets_by_ent$bracket_idx)
t10k_of_100k_scores = get_max_scores(t10k_of_100k_brackets)
t10k_of_100k_ent = get_entropy_df(t10k_of_100k_brackets)
# plot_scores(t10k_of_100k_scores, "t10k_of_100k", title="Top 10K of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(t10k_of_100k_ent, "t10k_of_100k", title="Top 10K of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

#####################################
### all brackets with <=3 upsets ###
#####################################

n = 63
upsets_0 = matrix(data=0,nrow=1,ncol=n)
upsets_1 = diag(n)
upsets_2 = matrix(nrow=n*(n-1)/2, ncol=n)
upsets_3 = matrix(nrow=n*(n-1)*(n-2)/6, ncol=n)
k = 1
for (i in 1:n) {
  for (j in 1:n) {
    if (j < i) {
      upsets_2[k, ] = upsets_1[i,] + upsets_1[j,]
      k = k+1
    }
  }
}
k = 1
for (i in 1:n) {
  for (j in 1:n) {
    for (m in 1:n) {
      if (i < j & j < m) {
        upsets_3[k, ] = upsets_1[i,] + upsets_1[j,] + upsets_1[m,]
        k = k+1
      }
    }
  }
}
upsets_012 = 1 - rbind(upsets_0, upsets_1, upsets_2)
upsets_013 = 1 - rbind(upsets_0, upsets_1, upsets_2, upsets_3)
dim(upsets_012)
dim(upsets_013)


get_bracket_with_specified_upset <- function(upsets_012) {
  start_of_round = start_of_tourney
  N = dim(upsets_012)[1]
  start_of_round = do.call("rbind", replicate(N, start_of_round, simplify = FALSE)) %>% mutate(bracket_idx = 1+floor((row_number()-1)/64) )
  round = 1
  ##all_rounds = list(start_of_round)
  all_rounds = tibble() ##tibble(start_of_round)
  
  while (round <= 6) {
    print(round)
    team1s = start_of_round %>% filter(row_number() %% 2 == 1)
    team2s = start_of_round %>% filter(row_number() %% 2 == 0)
    matchups = cbind(team1s$team_idx, team2s$team_idx)
    matchups = t(apply(matchups,1,sort))
    matchup_probs = apply(matchups, MARGIN=1, FUN=P)
    match_idxs = if (round==1) {1:32} else if (round==2) {33:48} else if (round==3) {49:56} else if (round==4) {57:60} else if (round==5) {61:62} else if (round==6) {63}
    matchup_results = as.vector(t(upsets_012[, match_idxs])) 
    ### log probability of this round occuring
    resulting_game_probs = matchup_results*matchup_probs + (1-matchup_results)*(1-matchup_probs)
    # log_probs = log(resulting_game_probs)
    ### winning teams of this round
    matchup_results_ = cbind(1:length(matchup_results), 2 - matchup_results)
    winners = matchups[matchup_results_]
    
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


# #### all brackets with <=2 upsets
# all_brackets_012_upsets = get_bracket_with_specified_upset(upsets_012)
# dim(all_brackets_012_upsets)[1]/63
# 
# 
# # u012_scores = get_max_scores(all_brackets_012_upsets)
# # u012_ent = get_entropy_df(all_brackets_012_upsets)
# # plot_scores(u012_scores, "u012", title="all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
# # plot_entropy_hist(u012_ent, "u012", title="all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)
# 
# u012_t100_brackets_by_ent = get_top_K_brackets(100, all_brackets_012_upsets)
# u012_t100_brackets = all_brackets_012_upsets %>% filter(bracket_idx %in% u012_t100_brackets_by_ent$bracket_idx)
# u012_t100_scores = get_max_scores(u012_t100_brackets)
# u012_t100_ent = get_entropy_df(u012_t100_brackets)
# # plot_scores(u012_t100_scores, "u012_t100", title="Top 100 of all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
# # plot_entropy_hist(u012_t100_ent, "u012_t100", title="Top 100 of all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)
# 
# u012_t1k_brackets_by_ent = get_top_K_brackets(10^3, all_brackets_012_upsets)
# u012_t1k_brackets = all_brackets_012_upsets %>% filter(bracket_idx %in% u012_t1k_brackets_by_ent$bracket_idx)
# u012_t1k_scores = get_max_scores(u012_t1k_brackets)
# u012_t1k_ent = get_entropy_df(u012_t1k_brackets)
# # plot_scores(u012_t1k_scores, "u012_t1k", title="Top 1k of all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
# # plot_entropy_hist(u012_t1k_ent, "u012_t1k", title="Top 1k of all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)




#### all brackets with <=3 upsets
all_brackets_013_upsets = get_bracket_with_specified_upset(upsets_013)
dim(all_brackets_013_upsets)[1]/63


# u013_scores = get_max_scores(all_brackets_013_upsets)
# u013_ent = get_entropy_df(all_brackets_013_upsets)
# plot_scores(u013_scores, "u013", title="all brackets with 3 or fewer upsets", savePlot=SAVE_PLOT)
# plot_entropy_hist(u013_ent, "u013", title="all brackets with 3 or fewer upsets",savePlot=SAVE_PLOT)

u013_t100_brackets_by_ent = get_top_K_brackets(100, all_brackets_013_upsets)
u013_t100_brackets = all_brackets_013_upsets %>% filter(bracket_idx %in% u013_t100_brackets_by_ent$bracket_idx)
u013_t100_scores = get_max_scores(u013_t100_brackets)
u013_t100_ent = get_entropy_df(u013_t100_brackets)
# plot_scores(u013_t100_scores, "u013_t100", title="Top 100 of all brackets with 3 or fewer upsets", savePlot=SAVE_PLOT)
# plot_entropy_hist(u013_t100_ent, "u013_t100", title="Top 100 of all brackets with 3 or fewer upsets",savePlot=SAVE_PLOT)

u013_t1k_brackets_by_ent = get_top_K_brackets(10^3, all_brackets_013_upsets)
u013_t1k_brackets = all_brackets_013_upsets %>% filter(bracket_idx %in% u013_t1k_brackets_by_ent$bracket_idx)
u013_t1k_scores = get_max_scores(u013_t1k_brackets)
u013_t1k_ent = get_entropy_df(u013_t1k_brackets)
# plot_scores(u013_t1k_scores, "u013_t1k", title="Top 1k of all brackets with 3 or fewer upsets", savePlot=SAVE_PLOT)
# plot_entropy_hist(u013_t1k_ent, "u013_t1k", title="Top 1k of all brackets with 3 or fewer upsets",savePlot=SAVE_PLOT)

u013_t10k_brackets_by_ent = get_top_K_brackets(10^4, all_brackets_013_upsets)
u013_t10k_brackets = all_brackets_013_upsets %>% filter(bracket_idx %in% u013_t10k_brackets_by_ent$bracket_idx)
u013_t10k_scores = get_max_scores(u013_t10k_brackets)
u013_t10k_ent = get_entropy_df(u013_t10k_brackets)
# plot_scores(u013_t10k_scores, "u013_t10k", title="Top 10k of all brackets with 3 or fewer upsets", savePlot=SAVE_PLOT)
# plot_entropy_hist(u013_t10k_ent, "u013_t10k", title="Top 10k of all brackets with 3 or fewer upsets",savePlot=SAVE_PLOT)

##############################  ############################## 



hr_100_a = sample_brackets_h_range(100, hL=47-5, hU=47+5)
hr_100_b = sample_brackets_h_range(100, hU=40)
hr_100_c = sample_brackets_h_range(100, hL=35-2, hU=35+2) #***f1
hr_100_d = sample_brackets_h_range(100, hL=37-2, hU=37+2)
hr_100_e = sample_brackets_h_range(100, hL=39-2, hU=39+2)
hr_100_f = sample_brackets_h_range(100, hL=41-2, hU=41+2)
hr_100_g = sample_brackets_h_range(100, hL=43-2, hU=43+2) #***f2
hr_100_h = sample_brackets_h_range(100, hL=43-1, hU=43+1)




hr_1k_a = sample_brackets_h_range(10^3, hL=47-2, hU=47+2)
hr_1k_b = sample_brackets_h_range(10^3, hU=40)
hr_1k_c = sample_brackets_h_range(10^3, hL=35-2, hU=35+2) #***f1
hr_1k_g = sample_brackets_h_range(10^3, hL=43-2, hU=43+2) #***f2
hr_1k_h = sample_brackets_h_range(10^3, hL=43-1, hU=43+1)



hr_100_a_scores = get_max_scores(hr_100_a)
hr_100_b_scores = get_max_scores(hr_100_b)
hr_100_c_scores = get_max_scores(hr_100_c)
hr_100_d_scores = get_max_scores(hr_100_d)
hr_100_e_scores = get_max_scores(hr_100_e)
hr_100_f_scores = get_max_scores(hr_100_f)
hr_100_g_scores = get_max_scores(hr_100_g)
hr_100_h_scores = get_max_scores(hr_100_h)
hr_1k_a_scores = get_max_scores(hr_1k_a)
hr_1k_b_scores = get_max_scores(hr_1k_b)
hr_1k_c_scores = get_max_scores(hr_1k_c)
hr_1k_g_scores = get_max_scores(hr_1k_g)
hr_1k_h_scores = get_max_scores(hr_1k_h)


mean(hr_100_a_scores[[1]]$f1)
mean(hr_100_b_scores[[1]]$f1)
mean(hr_100_c_scores[[1]]$f1)
mean(hr_100_d_scores[[1]]$f1)
mean(hr_100_e_scores[[1]]$f1)
mean(hr_100_f_scores[[1]]$f1)
mean(hr_100_g_scores[[1]]$f1)
mean(hr_100_h_scores[[1]]$f1)
mean(hr_1k_c_scores[[1]]$f1)
mean(hr_1k_g_scores[[1]]$f1)
mean(hr_1k_h_scores[[1]]$f1)
mean(r100_scores[[1]]$f1)
mean(t100_of_100k_scores[[1]]$f1)

mean(hr_100_a_scores[[1]]$f2)
mean(hr_100_b_scores[[1]]$f2)
mean(hr_100_c_scores[[1]]$f2)
mean(hr_100_d_scores[[1]]$f2)
mean(hr_100_e_scores[[1]]$f2)
mean(hr_100_f_scores[[1]]$f2)
mean(hr_100_g_scores[[1]]$f2)
mean(hr_100_h_scores[[1]]$f2)
mean(hr_1k_c_scores[[1]]$f2)
mean(hr_1k_g_scores[[1]]$f2)
mean(hr_1k_h_scores[[1]]$f2)
mean(r1k_scores[[1]]$f2)
mean(t1k_of_100k_scores[[1]]$f2)



############################## Grid Search ############################## 


# h_set = 42:43
# epsilon_set = c(1,2)
h_set = 35:59
epsilon_set = c(0.5, 1, 2.5, 5, 7.5, 10, Inf)
N = 100

f1_scores = matrix(nrow=length(h_set), ncol=length(epsilon_set))
f2_scores = matrix(nrow=length(h_set), ncol=length(epsilon_set))
rownames(f1_scores) = h_set; colnames(f1_scores) = epsilon_set;
rownames(f2_scores) = h_set; colnames(f2_scores) = epsilon_set;
for (i in 1:length(h_set)) {
  for (j in 1:length(epsilon_set)) {
    print("\n"); print(c(i,j)); print("\n");
    h = h_set[i]
    epsilon = epsilon_set[j]
    
    B_he_ent = r100k_ent %>% filter(h - epsilon <= log_prob_x & log_prob_x <= h + epsilon) %>%
      sample_n(size=min(N, n()), replace=FALSE)
      # filter(row_number() <= N)
    B_he = B_he_ent %>% left_join(r100k_brackets)
    B_he
    B_he_scores = get_max_scores(B_he, print_b=FALSE)
    
    f1_scores[i,j] = mean(B_he_scores[[1]]$f1)
    f2_scores[i,j] = mean(B_he_scores[[1]]$f2)
  }
}


##################
### More Plots ###
##################
# 
# write_csv(r100_brackets, paste0(plot_folder, "r100_brackets.csv"))
# write_csv(r100_scores, paste0(plot_folder, "r100_scores.csv"))
# write_csv(r100_ent, paste0(plot_folder, "r100_ent.csv"))
# write_csv(r1k_brackets, paste0(plot_folder, "r1k_brackets.csv"))
# write_csv(r1k_scores, paste0(plot_folder, "r1k_scores.csv"))
# write_csv(r1k_ent, paste0(plot_folder, "r1k_ent.csv"))
# write_csv(r10k_brackets, paste0(plot_folder, "r10k_brackets.csv"))
# write_csv(r10k_scores, paste0(plot_folder, "r10k_scores.csv"))
# write_csv(r10k_ent, paste0(plot_folder, "r10k_ent.csv"))
# 
# write_csv(t100_of_100k_brackets, paste0(plot_folder, "t100_of_100k_brackets.csv"))
# write_csv(t100_of_100k_scores, paste0(plot_folder, "t100_of_100k_scores.csv"))
# write_csv(t100_of_100k_ent, paste0(plot_folder, "t100_of_100k_ent.csv"))
# write_csv(t1k_of_100k_brackets, paste0(plot_folder, "t1k_of_100k_brackets.csv"))
# write_csv(t1k_of_100k_scores, paste0(plot_folder, "t1k_of_100k_scores.csv"))
# write_csv(t1k_of_100k_ent, paste0(plot_folder, "t1k_of_100k_ent.csv"))
# write_csv(t10k_of_100k_brackets, paste0(plot_folder, "t10k_of_100k_brackets.csv"))
# write_csv(t10k_of_100k_scores, paste0(plot_folder, "t10k_of_100k_scores.csv"))
# write_csv(t10k_of_100k_ent, paste0(plot_folder, "t10k_of_100k_ent.csv"))
# 
# write_csv(u013_t100_brackets, paste0(plot_folder, "u013_t100_brackets.csv"))
# write_csv(u013_t100_scores, paste0(plot_folder, "u013_t100_scores.csv"))
# write_csv(u013_t100_ent, paste0(plot_folder, "u013_t100_ent.csv"))
# write_csv(u013_t1k_brackets, paste0(plot_folder, "u013_t1k_brackets.csv"))
# write_csv(u013_t1k_scores, paste0(plot_folder, "u013_t1k_scores.csv"))
# write_csv(u013_t1k_ent, paste0(plot_folder, "u013_t1k_ent.csv"))
# write_csv(u013_t10k_brackets, paste0(plot_folder, "u013_t10k_brackets.csv"))
# write_csv(u013_t10k_scores, paste0(plot_folder, "u013_t10k_scores.csv"))
# write_csv(u013_t10k_ent, paste0(plot_folder, "u013_t10k_ent.csv"))



hist(r100_scores[[2]]$H1)

r100_scores[[2]] %>% ggplot() + geom_histogram(aes(x=H1))
r1k_scores[[2]] %>% ggplot() + geom_histogram(aes(x=H1))
r10k_scores[[2]] %>% ggplot() + geom_histogram(aes(x=H1))
# r100k_scores[[2]] %>% ggplot() + geom_histogram(aes(x=H1))


mean(r100_scores[[1]]$f1)
mean(r1k_scores[[1]]$f1)
mean(r10k_scores[[1]]$f1)
mean(t100_of_100k_scores[[1]]$f1)
mean(t1k_of_100k_scores[[1]]$f1)
mean(t10k_of_100k_scores[[1]]$f1)
mean(u013_t100_scores[[1]]$f1)
mean(u013_t1k_scores[[1]]$f1)
mean(u013_t10k_scores[[1]]$f1)

mean(r100_scores[[1]]$f2)
mean(r1k_scores[[1]]$f2)
mean(r10k_scores[[1]]$f2)
mean(t100_of_100k_scores[[1]]$f2)
mean(t1k_of_100k_scores[[1]]$f2)
mean(t10k_of_100k_scores[[1]]$f2)
mean(u013_t100_scores[[1]]$f2)
mean(u013_t1k_scores[[1]]$f2)
mean(u013_t10k_scores[[1]]$f2)



mean(r100_scores[[2]]$H1)
mean(r1k_scores[[2]]$H1)
mean(r10k_scores[[2]]$H1)
mean(t100_of_100k_scores[[2]]$H1)
mean(t1k_of_100k_scores[[2]]$H1)
mean(t10k_of_100k_scores[[2]]$H1)

sd(r100_scores[[2]]$H1)
sd(r1k_scores[[2]]$H1)
sd(r10k_scores[[2]]$H1)
sd(t100_of_100k_scores[[2]]$H1)
sd(t1k_of_100k_scores[[2]]$H1)
sd(t10k_of_100k_scores[[2]]$H1)


r100_scores[[3]] %>% ggplot() + geom_histogram(aes(x=H2))
r1k_scores[[3]] %>% ggplot() + geom_histogram(aes(x=H2))
r10k_scores[[3]] %>% ggplot() + geom_histogram(aes(x=H2))
# r100k_scores[[3]] %>% ggplot() + geom_histogram(aes(x=H2))

mean(r100_scores[[3]]$H2)
mean(r1k_scores[[3]]$H2)
mean(r10k_scores[[3]]$H2)
mean(t100_of_100k_scores[[3]]$H2)
mean(t1k_of_100k_scores[[3]]$H2)
mean(t10k_of_100k_scores[[3]]$H2)

sd(r100_scores[[3]]$H2)
sd(r1k_scores[[3]]$H2)
sd(r10k_scores[[3]]$H2)
sd(t100_of_100k_scores[[3]]$H2)
sd(t1k_of_100k_scores[[3]]$H2)
sd(t10k_of_100k_scores[[3]]$H2)


##############################  ############################## 

r100_scores$method = "r100"
t100_of_100k_scores$method = "t100_of_100k"
u013_t100_scores$method = "t100_of_u3"
hist1_100 = bind_rows(r100_scores, t100_of_100k_scores, u013_t100_scores) %>% 
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=f1, fill=method), alpha=0.5, position="identity", bins=30) +
  xlab("maximum number of correct picks") 
hist1_100
ggsave(paste0(plot_folder,"hist1_100.png"), hist1_100, width=6,height=6)

r1k_scores$method = "r1k"
t1k_of_100k_scores$method = "t1k_of_100k"
u013_t1k_scores$method = "t1k_of_u3"
hist1_1k = bind_rows(r1k_scores, t1k_of_100k_scores, u013_t1k_scores) %>% 
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=f1, fill=method), alpha=0.5, position="identity", bins=30) +
  xlab("maximum number of correct picks") 
hist1_1k
ggsave(paste0(plot_folder,"hist1_1k.png"), hist1_1k, width=6,height=6)

hist2_100 = bind_rows(r100_scores, t100_of_100k_scores, u013_t100_scores) %>% 
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=f2, fill=method), alpha=0.5, position="identity", bins=50) +
  xlab("maximum ESPN score") 
hist2_100
ggsave(paste0(plot_folder,"hist2_100.png"), hist2_100, width=6,height=6)

hist2_1k = bind_rows(r1k_scores, t1k_of_100k_scores, u013_t1k_scores) %>% 
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=f2, fill=method), alpha=0.5, position="identity", bins=50) +
  xlab("maximum ESPN score") 
hist2_1k
ggsave(paste0(plot_folder,"hist2_1k.png"), hist2_1k, width=6,height=6)

############################## entropy histograms ############################## 
r100_ent$method = "r100"
t100_of_100k_brackets_by_ent$method = "t100_of_100k"
u013_t100_brackets_by_ent$method = "t100_of_u3"
hist_ent_100 = bind_rows(r100_ent, t100_of_100k_brackets_by_ent, u013_t100_brackets_by_ent) %>%
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=log_prob_x, fill=method), alpha=0.5, position="identity", bins=50) +
  scale_x_continuous(name="entropy H(x)", breaks=seq(10,80,by=10),limits=c(30,70))
hist_ent_100

r1k_ent$method = "r1k"
t1k_of_100k_brackets_by_ent$method = "t1k_of_100k"
u013_t1k_brackets_by_ent$method = "t1k_of_u3"
hist_ent_1k = bind_rows(r1k_ent, t1k_of_100k_brackets_by_ent, u013_t1k_brackets_by_ent) %>%
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=log_prob_x, fill=method), alpha=0.5, position="identity", bins=50) +
  scale_x_continuous(name="entropy H(x)", breaks=seq(10,80,by=10),limits=c(30,70))
hist_ent_1k

r10k_ent$method = "r10k"
t10k_of_100k_brackets_by_ent$method = "t10k_of_100k"
u013_t10k_brackets_by_ent$method = "t10k_of_u3"
hist_ent_10k = bind_rows(r10k_ent, t10k_of_100k_brackets_by_ent, u013_t10k_brackets_by_ent) %>%
  ggplot() +
  theme(legend.position="none") +
  geom_histogram(aes(x=log_prob_x, fill=method), alpha=0.5, position="identity", bins=50) +
  scale_x_continuous(name="entropy H(x)", breaks=seq(10,80,by=10),limits=c(30,70))
hist_ent_10k

ggsave(paste0(plot_folder,"hist_ent_100.png"), hist_ent_100, width=6,height=6)
ggsave(paste0(plot_folder,"hist_ent_1k.png"), hist_ent_1k, width=6,height=6)
ggsave(paste0(plot_folder,"hist_ent_10k.png"), hist_ent_10k, width=6,height=6)

##############################  ############################## 



##################
### Rank Plots ###
##################

plot_rank <- function(comp_scores, str, title="", savePlot=SAVE_PLOT) {
  p1 = comp_scores %>% group_by(set, rk1) %>% summarise(count=n(),.groups="drop") %>%
        ggplot() + facet_wrap(~set) +
        geom_col(aes(x=rk1,y=count), fill="black") +
        labs(title=title, x="Rank the Strategies' Max Number of Correct Picks")
  p2 = comp_scores %>% group_by(set, rk2) %>% summarise(count=n(),.groups="drop") %>%
        ggplot() + facet_wrap(~set) +
        geom_col(aes(x=rk2,y=count), fill="black") +
        labs(title=title, x="Rank the Strategies' Max ESPN Scores")
  if (savePlot) {
    ggsave(paste0(plot_folder, "rank_hist_1_",str,"_.png"), p1, width=9, height=3)
    ggsave(paste0(plot_folder,"rank_hist_2_",str,"_.png"), p2, width=9, height=3)
  }
  p2
}

r100_scores = r100_scores %>% mutate(set = "100 random", trial=row_number())
t100_of_100k_scores = t100_of_100k_scores %>% mutate(set = "top 100 of 100k random", trial=row_number())
u012_t100_scores = u012_t100_scores %>% mutate(set = "top 100 with <=3 upsets", trial=row_number())
comp100 = bind_rows(r100_scores, t100_of_100k_scores, u012_t100_scores)
comp100 = comp100 %>% group_by(trial) %>% mutate(rk1 = rank(-f1), rk2 = rank(-f2)) %>% ungroup()
comp100 %>% arrange(trial,rk1)
# plot_rank(comp100, "100", title="Compare the 100 Bracket Strategies",savePlot=SAVE_PLOT)
plot_rank(comp100, "100", title="",savePlot=SAVE_PLOT)


r1k_scores = r1k_scores %>% mutate(set = "1k random", trial=row_number())
t1k_of_100k_scores = t1k_of_100k_scores %>% mutate(set = "top 1k of 100k random", trial=row_number())
u013_t1k_scores = u013_t1k_scores %>% mutate(set = "top 1k with <=3 upsets", trial=row_number())
comp1k = bind_rows(r1k_scores, t1k_of_100k_scores, u013_t1k_scores)
comp1k = comp1k %>% group_by(trial) %>% mutate(rk1 = rank(-f1), rk2 = rank(-f2)) %>% ungroup()
comp1k %>% arrange(trial,rk1)
# plot_rank(comp1k, "1k", title="Compare the 1k Bracket Strategies",savePlot=SAVE_PLOT)
plot_rank(comp1k, "1k", title="",savePlot=SAVE_PLOT)


r10k_scores = r10k_scores %>% mutate(set = "10k random", trial=row_number())
t10k_of_100k_scores = t10k_of_100k_scores %>% mutate(set = "top 10k of 100k random", trial=row_number())
u013_t1k_scores = u013_t10k_scores %>% mutate(set = "top 10k with <=3 upsets", trial=row_number())
comp10k = bind_rows(r10k_scores, t10k_of_100k_scores, u013_t1k_scores)
comp10k = comp10k %>% group_by(trial) %>% mutate(rk1 = rank(-f1), rk2 = rank(-f2)) %>% ungroup()
comp10k %>% arrange(trial,rk1)
plot_rank(comp10k, "10k", title="Compare the 10k Bracket Strategies",savePlot=SAVE_PLOT)


########################################
### Rare Brackets Prob. Calculations ###
########################################

# rand_brackets = r10k_ent %>% rename(Y = log_prob_x) %>% mutate(p = 2^(-Y))
rand_brackets = r100k_ent %>% rename(Y = log_prob_x) %>% mutate(p = 2^(-Y))

H = mean(rand_brackets$Y)
H
nu = var(rand_brackets$Y)
nu

quantile(rand_brackets$Y, c(.001,.01,.05,.1,.9,.95,.975,.99,.999 ))

percentile_of_value <- function(distr, values) {
  pov = ecdf(distr)(values)
  names(pov) = values
  pov
} 
percentile_of_value(rand_brackets$Y, c(32:63))


# pr_chernoff <- function(epsilon) {
#   g <- function(a) {
#     rb_a = rand_brackets %>% 
#       mutate( ee = exp(a*(Y-H))  ) %>%
#       mutate( ep = exp( log(p) + log(ee) )) 
#     a*epsilon -log(sum(rb_a$ep))
#   }
#   sup = optimize(g,lower = -10, upper = 10, 
#            maximum = TRUE, tol = .Machine$double.eps^0.25)$maximum
#   exp(-sup)
# }



#####################################
### n iid Bernoulli(p) coin flips ###
#####################################

# ### 63 iid ber(p) coin flips
# generate_flips <- function(n=63, p=.787, B=10^4) {
#   log_prob_xs = numeric(B)
#   for (b in 1:B) {
#     x = as.numeric(rbernoulli(n, p=p))
#     lp_x = log(x*p + (1-x)*(1-p), base=2)
#     log_prob_x = -sum(lp_x)
#     log_prob_xs[b] = log_prob_x
#   }
#   log_prob_xs
# }
# 
# ### as n -> infty, delta_n looks linear
# ns = seq(100,1000,by=100)
# delta_n_df = tibble()
# for (i in 1:length(ns)) {
#   print(i)
#   n = ns[i]
#   sample_n = generate_flips(n=n)
#   var_n = var(sample_n)
#   delta_n_df = bind_rows(delta_n_df, tibble(n=n, v=var_n))
# }
# plot(delta_n_df$v)
# m1 = lm(v~n, data=delta_n_df)
# summary(m1)
# coefficients(m1)[2]
# 
# var_flip <- function(p=.787) {
#   p*log(p,base=2)^2 + (1-p)*log(1-p,base=2)^2 - (p*log(p,base=2) + (1-p)*log(1-p,base=2))^2
# }
# var_flip()
# 
# ent_flip <- function(p=.787) {
#   -(p*log(p,base=2) + (1-p)*log(1-p,base=2))
# }
# ent_flip()*63
# 
# .596/63
# .596/63/.1^2
# 
# log_prob_xs = generate_flips()
# tibble(h=log_prob_xs) %>% ggplot() +
#   geom_histogram(aes(x=h,y=..density..), fill="black",bins=100) +
#   # xlim(c(0,1*10^(-11))) +
#   # ylim(c(0,1.5*10^(12))) +
#   geom_vline(aes(xintercept = mean(h)), color="dodgerblue2") +
#   theme(#axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()) +
#   xlab("H")
# mean(log_prob_xs)
# var(log_prob_xs)
# 
# -(ppp*log(ppp,base=2)+(1-ppp)*log(1-ppp,base=2))
# mean(log_prob_xs/nnn)
# var(log_prob_xs/nnn)
# -(ppp*log(ppp,base=2)+(1-ppp)*log(1-ppp,base=2))
# ccc = log(ppp,base=2)^2*ppp*(1-ppp)

















##########################################################
# entropy
# entropy_df = r10k_brackets %>% group_by(bracket_idx) %>% 
#   summarise(log_prob_x = -sum( log(prob,base=2) )) 
# entropy_df
# H = mean(entropy_df$log_prob_x)
# H
# nu = var(entropy_df$log_prob_x)
# nu
# var_bd <- function(epsilon) {
#   nu/epsilon^2
# }
# tibble(epsilon = seq(7,17,by=1)) %>%
#   ggplot(aes(x=epsilon)) +
#   stat_function(fun = var_bd, color = "blue")
# uniroot(function(e) {var_bd(e) - .1}, c(10,20))
# var_bd(15.37)
# log( (1-var_bd(15.37))*2^(H-15.37), base=2)
# H+15.37
# 
# epsilon = .001
# epsilon
# Delta = sqrt(nu/epsilon)
# Delta
# T_interval = c( (1-nu/epsilon^2)*2^(H-epsilon), 2^(H+epsilon) ) 
# T_interval
# prob_T = 1 - delta/epsilon^2
# prob_T
# ent <- function(p) {-p*log(p,base=2) - (1-p)*log(1-p,base=2)}
# ent_root <- function(p) {-p*log(p,base=2) - (1-p)*log(1-p,base=2) - H/63}
# uniroot(ent_root, c(.5, .8))
# uniroot(ent_root, c(.5, .8))$root
# 
# entropy_hist = entropy_df %>% ggplot() + 
#   geom_histogram(aes(x=log_prob_x,y=..density..), fill="black",bins=100) + 
#   # xlim(c(0,1*10^(-11))) + 
#   # ylim(c(0,1.5*10^(12))) +
#   geom_vline(aes(xintercept = mean(log_prob_x)), color="dodgerblue2") +
#   theme(#axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()) +
#   xlab("H")
# entropy_hist
# ggsave("entropy_hist.png", entropy_hist)



