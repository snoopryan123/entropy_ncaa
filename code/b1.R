source("2_main.R")

###########################
### Simulate N Brackets ###
###########################

start_of_tourney = D %>% arrange(game_id) %>% select(team_idx, game_id) %>% mutate(round = 1)

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
  all_rounds
}

reformat_simulated_brackets <- function(brackets) {
  left_join(brackets, D %>% select(team_idx, seed)) %>% 
    relocate(seed, .after=round) %>%
    arrange(bracket_idx,round,game_id) 
}

num_t = 100 #10^3
true_brackets0 = simulate_brackets(num_t)
true_brackets = reformat_simulated_brackets(true_brackets0)

get_max_scores <- function(brackets_set) {
  max_scores = tibble()
  for (b in 1:num_t) {
    print(b)
    true_b = true_brackets %>% filter(bracket_idx == b & round > 1)
    # true_b_teams = true_b$team_idx
    brackets1_b = tibble(brackets_set)
    ll = nrow(brackets_set) / nrow(true_b)
    brackets1_b$true_b_team = rep(true_b$team_idx, ll) 
    brackets1_b = brackets1_b %>% mutate(
      correct_pick = team_idx - true_b_team == 0,
      f1 = correct_pick*1,
      f2 = correct_pick*2^(round-2)*10,
      # f3 = correct_pick*( 2^(round-2) + seed)
    )
    scores_b = brackets1_b %>% group_by(bracket_idx) %>% 
      summarise(f1 = sum(f1), f2 = sum(f2))#, f3 = sum(f3))
    max_scores_b = scores_b %>% summarise(f1 = max(f1), f2 = max(f2))#, f3 = max(f3))
    max_scores = bind_rows(max_scores, max_scores_b)
  }
  max_scores
}

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
  pp = pp + geom_vline(xintercept = mm, color="dodgerblue2")
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

get_entropy_df <- function(brackets_set) {
  brackets_set %>% group_by(bracket_idx) %>% summarise(log_prob_x = -sum(log(prob,base=2) )) 
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



# brackets = simulate_brackets(5)

# takes ~30 sec to simulate 10k brackets
# brackets = simulate_brackets(10^4)

# # takes ~3 min to simulate 100k brackets, and 0.6 gB storage
# K = 10^5
# brackets1 = simulate_brackets(K)
# paste(object.size(brackets1)/(10^6), "mB")
# write_csv(brackets1, "../data/brackets_100k.csv")
# brackets1 = read_csv("../data/brackets_100k.csv")
# brackets1_ = reformat_simulated_brackets(brackets1)
# max_scores_100k = get_max_scores(brackets1_)
# p1 = plot_max_scores_hist(max_scores_100k, title="100k randomly sampled brackets")
# p1
# ggsave("p1.png", p1)

# r100_brackets_ = reformat_simulated_brackets(r10k_brackets)



######################################
### Try different sets of brackets ###
######################################

######################################
######### N Random Brackets ##########
######################################

# r100_brackets0 = simulate_brackets(10^2)
# r100_brackets = reformat_simulated_brackets(r100_brackets0)
# r100_scores = get_max_scores(r100_brackets)
# r100_ent = get_entropy_df(r100_brackets)
# plot_scores(r100_scores, "r100", title="100 randomly sampled brackets", savePlot=SAVE_PLOT)
# plot_entropy_hist(r100_ent, "r100", title="100 randomly sampled brackets",savePlot=SAVE_PLOT)

r1k_brackets0 = simulate_brackets(10^3)
r1k_brackets = reformat_simulated_brackets(r1k_brackets0)
r1k_scores = get_max_scores(r1k_brackets)
r1k_ent = get_entropy_df(r1k_brackets)
plot_scores(r1k_scores, "r1k", title="1k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(r1k_ent, "r1k", title="1k randomly sampled brackets",savePlot=SAVE_PLOT)

r10k_brackets0 = simulate_brackets(10^4)
r10k_brackets = reformat_simulated_brackets(r10k_brackets0)
r10k_scores = get_max_scores(r10k_brackets)
r10k_ent = get_entropy_df(r10k_brackets)
plot_scores(r10k_scores, "r10k", title="10k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(r10k_ent, "r10k", title="10k randomly sampled brackets",savePlot=SAVE_PLOT)

# H = mean(r10k_ent$log_prob_x)
# H
# nu = var(r10k_ent$log_prob_x)
# nu

r100k_brackets0 = simulate_brackets(10^5)
r100k_brackets = reformat_simulated_brackets(r100k_brackets0)
r100k_scores = get_max_scores(r100k_brackets)
r100k_ent = get_entropy_df(r100k_brackets)
plot_scores(r100k_scores, "r100k", title="100k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(r100k_ent, "r100k", title="100k randomly sampled brackets",savePlot=SAVE_PLOT)

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
t100_of_100k_brackets0 = r100k_brackets %>% filter(bracket_idx %in% t100_of_100k_brackets_by_ent$bracket_idx)
t100_of_100k_brackets = reformat_simulated_brackets(t100_of_100k_brackets0)
t100_of_100k_scores = get_max_scores(t100_of_100k_brackets)
t100_of_100k_ent = get_entropy_df(t100_of_100k_brackets) %>% arrange(log_prob_x)
plot_scores(t100_of_100k_scores, "t100_of_100k", title="Top 100 of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(t100_of_100k_ent, "t100_of_100k", title="Top 100 of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

t1k_of_100k_brackets_by_ent = get_top_K_brackets(1000, r100k_brackets)
t1k_of_100k_brackets0 = r100k_brackets %>% filter(bracket_idx %in% t1k_of_100k_brackets_by_ent$bracket_idx)
t1k_of_100k_brackets = reformat_simulated_brackets(t1k_of_100k_brackets0)
t1k_of_100k_scores = get_max_scores(t1k_of_100k_brackets)
t1k_of_100k_ent = get_entropy_df(t1k_of_100k_brackets)
plot_scores(t1k_of_100k_scores, "t1k_of_100k", title="Top 1K of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(t1k_of_100k_ent, "t1k_of_100k", title="Top 1K of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

t10k_of_100k_brackets_by_ent = get_top_K_brackets(10^4, r100k_brackets)
t10k_of_100k_brackets0 = r100k_brackets %>% filter(bracket_idx %in% t10k_of_100k_brackets_by_ent$bracket_idx)
t10k_of_100k_brackets = reformat_simulated_brackets(t10k_of_100k_brackets0)
t10k_of_100k_scores = get_max_scores(t10k_of_100k_brackets)
t10k_of_100k_ent = get_entropy_df(t10k_of_100k_brackets)
plot_scores(t10k_of_100k_scores, "t10k_of_100k", title="Top 10K of 100k randomly sampled brackets", savePlot=SAVE_PLOT)
plot_entropy_hist(t10k_of_100k_ent, "t10k_of_100k", title="Top 10K of 100k randomly sampled brackets",savePlot=SAVE_PLOT)

#####################################
### all brackets with <=2 upsets ###
#####################################

n = 63
upsets_0 = matrix(data=0,nrow=1,ncol=n)
upsets_1 = diag(n)
upsets_2 = matrix(nrow=n*(n-1)/2, ncol=n)
k = 1
for (i in 1:n) {
  for (j in 1:n) {
    if (j < i) {
      upsets_2[k, ] = upsets_1[i,] + upsets_1[j,]
      k = k+1
    }
  }
}
upsets_012 = 1 - rbind(upsets_0, upsets_1, upsets_2)
dim(upsets_012)



get_bracket_with_specified_upset <- function(upsets_012) {
  start_of_round = start_of_tourney
  N = dim(upsets_012)[1]
  start_of_round = do.call("rbind", replicate(N, start_of_round, simplify = FALSE)) %>% mutate(bracket_idx = 1+floor((row_number()-1)/64) )
  round = 1
  ##all_rounds = list(start_of_round)
  all_rounds = tibble() ##tibble(start_of_round)
  
  while (round <= 6) {
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
  all_rounds
}



all_brackets_012_upsets0 = get_bracket_with_specified_upset(upsets_012)
all_brackets_012_upsets = reformat_simulated_brackets(all_brackets_012_upsets0)
dim(all_brackets_012_upsets)[1]/63


u012_scores = get_max_scores(all_brackets_012_upsets)
u012_ent = get_entropy_df(all_brackets_012_upsets)
plot_scores(u012_scores, "u012", title="all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
plot_entropy_hist(u012_ent, "u012", title="all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)

u012_t100_brackets_by_ent = get_top_K_brackets(100, all_brackets_012_upsets)
u012_t100_brackets0 = all_brackets_012_upsets %>% filter(bracket_idx %in% u012_t100_brackets_by_ent$bracket_idx)
u012_t100_brackets = reformat_simulated_brackets(u012_t100_brackets0)
u012_t100_scores = get_max_scores(u012_t100_brackets)
u012_t100_ent = get_entropy_df(u012_t100_brackets)
plot_scores(u012_t100_scores, "u012_t100", title="Top 100 of all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
plot_entropy_hist(u012_t100_ent, "u012_t100", title="Top 100 of all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)

u012_t1k_brackets_by_ent = get_top_K_brackets(10^3, all_brackets_012_upsets)
u012_t1k_brackets0 = all_brackets_012_upsets %>% filter(bracket_idx %in% u012_t1k_brackets_by_ent$bracket_idx)
u012_t1k_brackets = reformat_simulated_brackets(u012_t1k_brackets0)
u012_t1k_scores = get_max_scores(u012_t1k_brackets)
u012_t1k_ent = get_entropy_df(u012_t1k_brackets)
plot_scores(u012_t1k_scores, "u012_t1k", title="Top 1k of all brackets with 2 or fewer upsets", savePlot=SAVE_PLOT)
plot_entropy_hist(u012_t1k_ent, "u012_t1k", title="Top 1k of all brackets with 2 or fewer upsets",savePlot=SAVE_PLOT)


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
    ggsave(paste0(plot_folder, "rank_hist_1_",str,"_.png"), p1, width=9, height=4)
    ggsave(paste0(plot_folder,"rank_hist_2_",str,"_.png"), p2, width=9, height=4)
  }
  p2
}

r100_scores = r100_scores %>% mutate(set = "r100", trial=row_number())
t100_of_100k_scores = t100_of_100k_scores %>% mutate(set = "t100_of_100k", trial=row_number())
u012_t100_scores = u012_t100_scores %>% mutate(set = "u012_t100", trial=row_number())
comp100 = bind_rows(r100_scores, t100_of_100k_scores, u012_t100_scores)
comp100 = comp100 %>% group_by(trial) %>% mutate(rk1 = rank(-f1), rk2 = rank(-f2)) %>% ungroup()
comp100 %>% arrange(trial,rk1)
plot_rank(comp100, "100", title="Compare the 100 Bracket Strategies",savePlot=SAVE_PLOT)


r1k_scores = r1k_scores %>% mutate(set = "r1k", trial=row_number())
t1k_of_100k_scores = t1k_of_100k_scores %>% mutate(set = "t1k_of_100k", trial=row_number())
u012_t1k_scores = u012_t1k_scores %>% mutate(set = "u012_t1k", trial=row_number())
comp1k = bind_rows(r1k_scores, t1k_of_100k_scores, u012_t1k_scores)
comp1k = comp1k %>% group_by(trial) %>% mutate(rk1 = rank(-f1), rk2 = rank(-f2)) %>% ungroup()
comp1k %>% arrange(trial,rk1)
plot_rank(comp1k, "1k", title="Compare the 1k Bracket Strategies",savePlot=SAVE_PLOT)


r10k_scores = r10k_scores %>% mutate(set = "r10k", trial=row_number())
t10k_of_100k_scores = t10k_of_100k_scores %>% mutate(set = "t10k_of_100k", trial=row_number())
comp10k = bind_rows(r10k_scores, t10k_of_100k_scores)
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


pr_chernoff <- function(epsilon) {
  g <- function(a) {
    rb_a = rand_brackets %>% 
      mutate( ee = exp(a*(Y-H))  ) %>%
      mutate( ep = exp( log(p) + log(ee) )) 
    a*epsilon -log(sum(rb_a$ep))
  }
  sup = optimize(g,lower = -10, upper = 10, 
           maximum = TRUE, tol = .Machine$double.eps^0.25)$maximum
  exp(-sup)
}



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



