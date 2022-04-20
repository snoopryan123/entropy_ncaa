source("2_main.R")


######################################
#########  bracket sampling ##########
######################################

sample_h_chalky_brackets <- function(N, h) {
  ### sample N brackets x with H(x) <= h
  N_sample_at_a_time = 10^4 ##round(log(N, base=10))
  result = tibble()
  num_sampled = 0
  while (num_sampled < N) {
    print(num_sampled)
    S = simulate_brackets(N_sample_at_a_time)
    keep = get_entropy_df(S) %>% filter(log_prob_x <= h)
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
get_S_scores <- function(S1,S2) {
  S12 = tibble(S1)
  S12$true_team_idx = S2$team_idx
  S12 = S12 %>% mutate(
    correct_pick = team_idx - true_team_idx == 0,
    f1 = correct_pick*1,
    f2 = correct_pick*2^(round-2)*10,
    # f3 = correct_pick*( 2^(round-2) + seed)
  )
  S12 = S12 %>% group_by(bracket_idx) %>% summarise(f1 = sum(f1), f2 = sum(f2))
  S12
}
# plot_f1_hist <- function(S, h_str, savePlot=FALSE) {
#   pp = S %>% ggplot() +
#     geom_histogram(aes(x=f1),bins=100, fill="black") +
#     geom_vline(aes(xintercept=mean(f1)), color="dodgerblue2", size=1) +
#     xlab("number of correct picks")
#   if (savePlot) {
#     ggsave(paste0(plot_folder, "S1_h=",h_str,".png"), pp, width=6,height=6)
#   }
#   pp
#
# }
# plot_f2_hist <- function(S, h_str, savePlot=FALSE) {
#   pp = S %>% ggplot() +
#     geom_histogram(aes(x=f2), bins=500, fill="black") +
#     geom_vline(aes(xintercept=mean(f2)), color="dodgerblue2", size=1) +
#     xlab("ESPN score")
#   if (savePlot) {
#     ggsave(paste0(plot_folder, "S2_h=",h_str,".png"), pp, width=6,height=6)
#   }
#   pp
# }

S_X.1 = simulate_brackets(10^3)
S_X.2 = simulate_brackets(10^3)
S_X.12 = get_S_scores(S_X.1,S_X.2)
write_csv(S_X.12, paste0(plot_folder,"S_X.12_1k.csv"))

### sample 10k brackets with H <= 41
S_41.1 = sample_h_chalky_brackets(10^3, 41)
S_X.3 = simulate_brackets(10^3)
S_41X = get_S_scores(S_41.1,S_X.3)
write_csv(S_41X,  paste0(plot_folder,"S_41X_1k.csv"))

### sample 10k brackets with H <= 38
S_38.1 = sample_h_chalky_brackets(10^3, 38)
S_X.4 = simulate_brackets(10^3)
S_38X = get_S_scores(S_38.1,S_X.4)
write_csv(S_38X,  paste0(plot_folder,"S_38X_1k.csv"))

### sample 10k brackets with H <= 36
S_36.1 = sample_h_chalky_brackets(10^3, 36)
S_X.5 = simulate_brackets(10^3)
S_36X = get_S_scores(S_36.1,S_X.5)
write_csv(S_36X,  paste0(plot_folder,"S_36X_1k.csv"))

### sample 10k brackets with H <= 35
S_35.1 = sample_h_chalky_brackets(10^3, 35)
S_X.6 = simulate_brackets(10^3)
S_35X = get_S_scores(S_35.1,S_X.6)
write_csv(S_35X,  paste0(plot_folder,"S_35X_1k.csv"))


# ### sample 10k brackets with H <= 35
# S_38.1 = sample_h_chalky_brackets(10^4, 35)
# S_X.4 = simulate_brackets(10^4)
# S_38X = get_S_scores(S_38.1,S_X.4)
# plot_f1_hist(S_38X)
# plot_f2_hist(S_38X)
# # write_csv(S_38X, "S_38X.csv")



# plot_f1_hist(S_X.12, h_str="all", savePlot=SAVE_PLOT)
# plot_f2_hist(S_X.12, h_str="all", savePlot=SAVE_PLOT)
# plot_f1_hist(S_41X)
# plot_f2_hist(S_41X)
# plot_f1_hist(S_38X)
# plot_f2_hist(S_38X)


######################################################
########  E[max f(x|tau)] as a function of n #########
######################################################

M = 2*10^4
h1tau = sample_brackets_h_range(M)
h1r = sample_brackets_h_range(M)
S1r = get_S_scores(h1r, h1tau) 
h1a = sample_brackets_h_range(M, hU=46)
S1a = get_S_scores(h1a, h1tau) 

F_hat <- function(S, str="") {
  k1 = seq(0, 63, by=1)
  tib1 = tibble(k = k1, F_hat_k = ecdf(S$f1)(k1)) %>%
    mutate(scoring_function="f1", bracket_set=str) %>%
    mutate(F_hat_k = lag(F_hat_k, default=1)) ### need 1{f < k}, not 1{f <= k}
  k2 = seq(0, 1920, by=1)
  tib2 = tibble(k = k2, F_hat_k = ecdf(S$f2)(k2)) %>%
    mutate(scoring_function="f2", bracket_set=str) %>%
    mutate(F_hat_k = lag(F_hat_k, default=1)) ### need 1{f < k}, not 1{f <= k}
  bind_rows(tib1, tib2)
}

# F_hat(S1r, "random N")

E <- function(ns, S, str) {
  Fh = F_hat(S, str) 
  # data.frame(Fh)
  E_final = tibble()
  for (n in ns) {
    E_k = Fh %>% group_by(scoring_function) %>% mutate(E_k = 1 - exp(n*log(F_hat_k))) #F_hat_k^n) 
    # data.frame(E_k)
    E_final = bind_rows(E_final, E_k %>% summarise(E = sum(E_k), n=n, bracket_set=str))
  }
  E_final %>% arrange(scoring_function, n)
}


ns = c(1,10,10^2,10^3,10^4,10^5)
E(ns, S1r, "random N")
E(ns, S1a, "random N, H <= 46")



###########################
#########  PLOTS ##########
###########################

# F_hat_tib = bind_rows(F_hat_f1(S_X.12, "all"), 
#                       F_hat_f1(S_41X, "h<=41"), 
#                       F_hat_f1(S_38X, "h<=38"),
#                       F_hat_f1(S_36X, "h<=36"))
# 
# F_hat_tib 
# 
# F_hat_tib %>% ggplot() +
#   facet_wrap(~scoring_function, scales = "free") +
#   geom_line(aes(x = k, y=F_hat, color=bracket_set))
# 









