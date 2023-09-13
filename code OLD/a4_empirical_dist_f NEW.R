source("a2_main.R")

######################################################
########  E[max f(x|tau)] as a function of n #########
######################################################

M = 10^4
h1tau = sample_brackets_h_range(M)
h1r = sample_brackets_h_range(M)
S1r = get_S_scores(h1r, h1tau) 
h1a = sample_brackets_h_range(M, hU=46)
S1a = get_S_scores(h1a, h1tau) 

h1b = sample_brackets_h_range(M, hU=41)
S1b = get_S_scores(h1b, h1tau) 

h1c = sample_brackets_h_range(M, hL=40, hU=44)
S1c = get_S_scores(h1c, h1tau) 

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


# ns = c(1,10,10^2,10^3,10^4,10^5)
ns = c(1:1000, seq(10^3,10^5,by=1000))
E1r = E(ns, S1r, "random")
E1a = E(ns, S1a, "random, H <= 46")
E1b = E(ns, S1b, "random, H <= 41")
E1c = E(ns, S1c, "random, 40 <= H <= 44")


plot_Escore_n <- function(Escores) {
  Escores %>%
    ggplot(aes(x=log(n,base=10), y=E, color=bracket_set)) +
    facet_wrap(~scoring_function, scales="free_y", 
               labeller = as_labeller(c(
                 f1 = "Number of correct game winners",
                 f2 = "ESPN score"))) +
    geom_line(size=0.75) +
    # scale_y_continuous(breaks=c(seq(0,1900,by=100),1920) )
    # scale_color_discrete(name = "bracket set", labels = c(
    #     "random",
    #     "random, H \u2264 41",
    #     "random, H \u2264 46"
    # )) +
    labs(x=TeX("$$log_{10}(n)$$"),
         y="Expected Maximum Score")
}
ppp1 = plot_Escore_n(bind_rows(E1r,E1a, E1b, E1c))
ppp1
###ggsave("expected_score_vs_n.png", ppp1, width=10, height=4)

##################################################################

#######################################################
### Grid Search: E[max f(x|tau)] as a function of n ###
#######################################################

M = 10^3
ns = c(1:1000, seq(10^3,10^5,by=1000))

h_set = seq(40,60,by=5)#40:60
epsilon_set = c(2,5,8)#1:7 

get_E <- function(h_set, epsilon_set) {
  h1_tau = sample_brackets_h_range(M)
  h1_random = sample_brackets_h_range(M)
  S1_random = get_S_scores(h1_random, h1_tau) 
  E1_random = E(ns, S1_random, "random")
  u013_t1k_brackets
  S1_u013_t1k_brackets = get_S_scores(u013_t1k_brackets, h1_tau) 
  E1_u013_t1k_brackets = E(ns, S1_u013_t1k_brackets, "top 1k brackets with <=3 upsets")
  E_tib = bind_rows(E1_random, E1_u013_t1k_brackets) #tibble(E1_random)
  for (i in 1:length(h_set)) {
    for (j in 1:length(epsilon_set)) {
      h = h_set[i]
      e = epsilon_set[j]
      hL = h-e
      hU = h+e
      print("***"); print(c(h,e)); print("***");
      h1_ij = sample_brackets_h_range(M, hL=hL, hU=hU)
      S1_ij = get_S_scores(h1_ij, h1_tau) 
      E1_ij = E(ns, S1_ij, paste0("random, ", hL,"<=H<=",hU))
      E_tib = bind_rows(E_tib, E1_ij)
    }
  }
  E_tib
}

E1 = get_E(h_set = seq(40,60,by=5), epsilon_set = c(2,5,8))

E2 = get_E(h_set = seq(37,47,by=2), epsilon_set = c(2,4,6))

E3 = get_E(h_set = seq(35,41,by=1), epsilon_set = c(2,4))

E4 = get_E(h_set = seq(36,38,by=1), epsilon_set = c(1,2,3,4))


p1 = plot_Escore_n(E1)
p1
ggsave("expected_score_vs_n_1.png", p1, width=10, height=4)


p2 = plot_Escore_n(E2)
p2
ggsave("expected_score_vs_n_2.png", p2, width=11, height=5)

p3 = plot_Escore_n(E3)
p3
ggsave("expected_score_vs_n_3.png", p3, width=11, height=5)

p4 = plot_Escore_n(E4)
p4
ggsave("expected_score_vs_n_4.png", p4, width=11, height=5)


#######################################################
### overlap histograms ###
#######################################################




d1 <- function(x1,x2) {
  sum(x1$team_idx - x2$team_idx != 0)
}

calculate_dist_matrix <- function(bracket_set) {
  d1 <- function(x1,x2) {
    sum(x1$team_idx - x2$team_idx != 0)
  }
  ### bracket_set is a tibble of brackets {x_i}
  ### d is a distance metric function d(x1,x2)
  bs = unique(bracket_set$bracket_idx)
  lbs = length(bs)
  d1s = matrix(nrow=lbs,ncol=lbs)
  rownames(d1s) = bs
  colnames(d1s) = bs
  for (i in 1:(lbs-1)) { 
    for (j in (i+1):lbs) {
      print(c(i,j))
      bi = bs[i]
      bj = bs[j]
      xi = bracket_set %>% filter(bracket_idx == bi)
      xj = bracket_set %>% filter(bracket_idx == bj)
      d1s[i,j] = d1(xi, xj)
    }
  }
  d1s
}

dm_1r = calculate_dist_matrix(r100_brackets)
dm_1u3 = calculate_dist_matrix(u013_t100_brackets)

r.38.42.brackets = sample_brackets_h_range(100,hL=38,hU=42)
dm_1r.38.42 = calculate_dist_matrix(r.38.42.brackets)


max(dm_1r, na.rm=TRUE)
mean(dm_1r, na.rm=TRUE)

max(dm_1u3, na.rm=TRUE)
mean(dm_1u3, na.rm=TRUE)

max(dm_1r.38.42, na.rm=TRUE)
mean(dm_1r.38.42, na.rm=TRUE)




