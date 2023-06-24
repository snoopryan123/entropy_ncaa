
#####################################################
### Load real horse racing data from Belmont Park ###
#####################################################

source("../a0_loadStuff.R")
output_folder = "plots/"

# Set display options to remove scientific notation
options(scipen = 999)

### racing data from Belmont Park 5/21/2023
D = read_csv("df_Belmont_5-21-23.csv")
### pick 6 with $38k carryover in races 4-9
D = D %>% filter(race_num %in% 4:9) %>% mutate(race_num = race_num - 3)
### https://www.nyra.com/belmont/news/pick-6-carryover-of-$38k-on-sunday-at-belmont-park
### https://entries.horseracingnation.com/entries-results/belmont-park/2023-05-21#race-4

### check
D %>% group_by(race_num) %>% 
  summarise(total_prob = sum(probs), total_odds = sum(1/ML_decimal_odds), num_horses = n())

# total_payout = 181274
# two_dollar_payout = 980

############################################################
### fixed probability matrix P from the real horse races ###
############################################################

### get P implied by Vegas from real horse racing data
### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
P = matrix(
  nrow = (D %>% group_by(race_num) %>% summarise(num_horses = n()) %>% summarise(max_num_horses = max(num_horses)))$max_num_horses,
  ncol = D %>% group_by(race_num) %>% summarise() %>% nrow()
)
colnames(P) = paste("race",1:ncol(P)) #sort(unique(D$race_num))
rownames(P) = paste("horse",1:nrow(P))
for (j in 1:ncol(P)) {
  for (i in 1:nrow(P)) {
    D_ij = D %>% filter(race_num == j & PP == i)
    if (nrow(D_ij) > 0) {
      P[i,j] = D_ij$probs
    } else {
      P[i,j] = 0
    }
  }
}
rm(i); rm(j)
colSums(P)
P

#############################
### lambda-chalky tickets ###
#############################

naively_chalky_P <- function(lambda_opp, P) {
  ### lambda_opp in [0,1] is a tilt parameter measuring how chalky the strategy is
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### output a probability matrix R such that R[i,j] is the probability that one picks horse i to win race j
  
  ### sort each race by p
  P_sorted <- apply(P, MARGIN=2, sort, decreasing=T)
  ### get the number of opp. "favorites" in each race
  P_diffs = abs(apply(P_sorted, MARGIN = 2, diff))
  P_diffs1 = P_diffs
  m = colSums(P != 0)
  P_diffs1[cbind(pmin(unname(m), max(m)-1), 1:ncol(P))] = 0
  f_mat = apply(P_diffs1, MARGIN = 2, function(x) x == max(x))
  f_vec = unname(apply(f_mat, MARGIN = 2, function(x) first(which(x))))
  ### get opp prob matrix
  R_sorted_lst = lapply(1:ncol(P), function(i) c(rep(1, f_vec[i]), rep(0, nrow(P) - f_vec[i])))
  names(R_sorted_lst) = paste0("j", 1:ncol(P))
  R_sorted = as.matrix(bind_cols(R_sorted_lst))
  colnames(R_sorted)=colnames(P)
  R_sorted = apply(R_sorted, MARGIN = 2, function(x) x/sum(x))
  C_opp = matrix(0, nrow=nrow(P), ncol=ncol(P), dimnames=dimnames(P))
  for (i in 1:ncol(P)) {
    C_opp[order(P[,i], decreasing = T),i] = R_sorted[,i]
  }
  # P
  # C_opp
  ### lambda_opp-chalky opp matrix
  lambda_opp*C_opp + (1-lambda_opp)*P
}
# ### check
# P
# naively_chalky_P(lambda_opp=0, P)
# naively_chalky_P(lambda_opp=1/2, P)
# colSums(naively_chalky_P(lambda_opp=1/2, P))
# naively_chalky_P(lambda_opp=3/4, P)
# naively_chalky_P(lambda_opp=1, P)

lambda_chalky_P <- function(lambda, index_a, P) {
  ### lambda in [0,1] is a tilt parameter measuring how chalky our strategy is
  ### index_a in 1,...,max(m) is a cutoff parameter indicating which horses we want to tilt upwards or downwards
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### output a probability matrix Q such that Q[i,j] is the probability that we pick horse i to win race j
  
  s = ncol(P) # number of horse races
  m = colSums(P != 0) # number of horses in each race
  max_m = max(m)
  if (!(1 <= index_a & index_a <= max_m)) {
    stop(paste0("index_a = ", index_a, " is invalid."))
  }
  
  max_Ps = apply(P, MARGIN=2, max)
  min_Ps = apply(P, MARGIN=2, min)
  # quantile_Ps = apply(P, MARGIN=2, quantile, probs = 0.4)
  a = numeric(s)
  for (j in 1:s) {
    as_j = rev(seq(min_Ps[j], max_Ps[j], length.out=max_m))
    a[j] = as_j[index_a]
  }
  
  C_a = matrix(0, nrow=nrow(P), ncol=ncol(P), dimnames=dimnames(P))
  for (j in 1:s) {
    C_a[,j] = P[,j] * (P[,j] >= a[j])
  }
  C_a
  Q = lambda*C_a + (1-lambda)*P
  Q = apply(Q, MARGIN=2, function(x) x/sum(x))
  colSums(Q)
  return(Q)
}
# ### check
# P
# lambda_chalky_P(lambda = 0, index_a = 1, P)
# lambda_chalky_P(lambda = 0, index_a = 15, P)
# lambda_chalky_P(lambda = 1, index_a = 1, P)
# lambda_chalky_P(lambda = 1, index_a = 3, P)
# lambda_chalky_P(lambda = 1, index_a = 5, P)
# lambda_chalky_P(lambda = 1, index_a = 7, P)
# lambda_chalky_P(lambda = 1, index_a = 9, P)
# lambda_chalky_P(lambda = 1, index_a = 11, P)
# lambda_chalky_P(lambda = 1, index_a = 15, P)
# lambda_chalky_P(lambda = 1, index_a = 1, P)
# lambda_chalky_P(lambda = 0.75, index_a = 1, P)
# colSums(lambda_chalky_P(lambda = 0.75, index_a = 1, P))
# lambda_chalky_P(lambda = 0.5, index_a = 1, P)
# lambda_chalky_P(lambda = 0.25, index_a = 1, P)
# lambda_chalky_P(lambda = 0, index_a = 1, P)

########################################################
### Expected Profit Procedure, vectorized over n,k,C ###
########################################################

P_tau <- function(tau,P) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### P is a matrix such that P[i,j] is the true probability that horse i wins race j
  s = length(tau) # number of horse races
  P_prod = prod(sapply(1:s, function(j) P[tau[j],j]))
  P_prod
}
# ### check
# P_tau(tau=c(4,3,1,3,4,2),P) ## higher prob
# P_tau(tau=c(1,5,7,1,5,8),P) ## lower prob

P_W_neq_0_given_tau <- function(tau,Q,ns) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### Q is a matrix in which Q[i,j] is the probability that we submit a ticket in which horse i wins in race j
  ### ns is a vector over n, where n is our number of submitted tickets
  s = length(tau) # number of horse races
  Q_prod = prod(sapply(1:s, function(j) Q[tau[j],j]))
  result = 1 - (1 - Q_prod)^ns
  names(result) = paste0("n=",ns)
  result
}
# # ### check
# tau = c(4,3,1,3,4,2)
# P_W_neq_0_given_tau(tau,Q=P,ns=10^(1:6))

E_W_opp_given_tau <- function(tau,R,ks) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### R is a matrix in which R[i,j] is the probability that an opponent submits a ticket in which horse i wins in race j
  ### ks is a vector over k, where k is our opponents' number of submitted tickets
  s = length(tau) # number of horse races
  R_prod = prod(sapply(1:s, function(j) R[tau[j],j]))
  result = ks * R_prod
  names(result) = paste0("k=",ks)
  result
}
# ### check
# tau = c(4,3,1,3,4,2)
# E_W_opp_given_tau(tau,R=P,k=10^(1:6))

E_profit_term_given_tau <- function(tau,P,Q,ns,R,ks,matrix_version=TRUE) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### P is a matrix such that P[i,j] is the true probability that horse i wins race j
  ### Q is a matrix in which Q[i,j] is the probability that we submit a ticket in which horse i wins in race j
  ### ns is a vector over n, where n is our number of submitted tickets
  ### R is a matrix in which R[i,j] is the probability that an opponent submits a ticket in which horse i wins in race j
  ### ks is a vector over k, where k is our opponents' number of submitted tickets
  tP =  P_tau(tau,P)
  tQ = P_W_neq_0_given_tau(tau,Q,ns)
  tR = 1 / (1 + E_W_opp_given_tau(tau,R,ks))
  Emat = outer(tQ,tR) * tP
  if (matrix_version) {
    return(Emat)
  }
  
  # # browser()
  # Emat = as.data.frame(Emat)
  # Emat$n = ns
  # Emat %>% 
  #   pivot_longer(-n, values_to="E_W_ratio") %>%
  #   mutate(k = as.numeric(str_sub(name,3))) %>% 
  #   select(-name) %>% 
  #   relocate(k, .after=n)
}
# ### check
# tau = c(4,3,1,3,4,2)
# E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=T)
# all(apply(E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=T), 
#           MARGIN=2, diff) > 0) ### check columns are monotonic increasing
# all(apply(E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=T), 
#           MARGIN=1, diff) < 0) ### check row are monotonic decreasing
# ###E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=F)

### enumerate all possible combinations of tau
all_combos_of_tau <- function(P) {
  ### P is a matrix such that P[i,j] is the true probability that horse i wins race j
  
  ### m = (m_1,...,m_s), where m_j is the number of horses in race j,
  ### which is the number of nonzero entries in P[,j]
  m = colSums(P != 0) 
  s = length(m) # number of horse races
  lst = list()
  for (j in 1:s) {
    lst[[j]] = 1:m[j]
  }
  all_taus = expand.grid(lst) %>% tibble()
  names(all_taus) = paste0("tau_",1:s)
  all_taus
}
# ### check
# all_combos_of_tau(P)

total_prize_money <- function(ns,ks,Cs,alphas) {
  ### ns is a vector over n, where n is our number of submitted tickets
  ### ks is a vector over k, where k is our opponents' number of submitted tickets
  ### C is a vector over C, where C is the carryover
  ### alpha sis a vector over alpha, where alpha is the track take
  ### output the total prize money
  tibble(expand.grid(n=ns,k=ks,C=Cs,alpha=alphas)) %>% mutate(t = C+(n+k)*(1-alpha)) 
}
# ### check
# T_mat = total_prize_money(ns=10^(1:6),ks=10^(1:6),Cs=c(0,2500,38000,1e5),alphas=c(0,0.05,0.18))
# T_mat

E_W_ratio <- function(P,Q,R,ns,ks,print_every_n=1e5) {
  all_taus = all_combos_of_tau(P)
  RESULT = 0
  # browser()
  for (i in 1:nrow(all_taus)) {
    tau = as.numeric(all_taus[i,])
    if ((i-1) %% print_every_n == 0) print(paste0("iteration i=",i,"/",nrow(all_taus), " tau=",paste(tau,collapse=" ")))
    RESULT = RESULT + E_profit_term_given_tau(tau,P,Q,ns,R,ks,matrix_version = TRUE)
  }
  RESULT
}
# ### check
# ex_ew1 = E_W_ratio(P,Q=P,R=P,ns=10^(1:6),ks=10^(1:6)) ### takes abt 1 minute
# R_unif = matrix(1/colSums(P!=0), nrow=nrow(P), ncol=ncol(P), byrow=TRUE)*(P!=0)
# ex_ew2 = E_W_ratio(P,Q=P,R=R_unif,ns=10^(1:6),ks=10^(1:6)) ### takes abt 1 minute
# ex_ew1
# ex_ew2
# total_prize_money(ns=10^(1:6),ks=10^(1:6),C=0,alpha=0)

E_profit <- function(P,Q,R,ns,ks,Cs,alphas,print_every_n=1e5) {
  EWR = E_W_ratio(P,Q,R,ns,ks,print_every_n=print_every_n)
  # EWR = ex_ew1
  # EWR = ex_ew2
  # apply(EWR, 1, function(row) all(diff(row) <= 0)) # Check if rows are decreasing
  # apply(EWR, 2, function(col) all(diff(col) >= 0)) # Check if cols are increasing
  
  # browser()
  EWR1 = as.data.frame(EWR) %>% 
    mutate(n = ns) %>%
    pivot_longer(-n, values_to="E_W_ratio") %>%
    mutate(k = as.numeric(str_sub(name,3))) %>% 
    select(-name) %>% 
    relocate(k, .after=n)
  
  T_mat = total_prize_money(ns,ks,Cs,alphas)
  df_E_profit = left_join(EWR1, T_mat) %>% mutate(E_profit = t*E_W_ratio - n)
  df_E_profit
}
### check
# E_profit(P,Q=P,R=P,ns=10^(1:6),ks=10^(1:6),Cs=c(0,2500,38000,1e5),alphas=c(0,0.05,0.18)) ### takes abt 1 minute
# E_profit(P,Q=P,R=P,ns=10^(1:6),ks=10^(1:6),Cs=c(0,2500,38000,1e5),alphas=c(0,0.05,0.18)) ### takes abt 1 minute




