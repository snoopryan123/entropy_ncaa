
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
D %>% group_by(race_num) %>% summarise(total_prob = sum(probs), total_odds = sum(1/ML_decimal_odds), num_horses = n())

# total_payout = 181274
# two_dollar_payout = 980

############################################################
### fixed parameters P,m,alpha from the real horse races ###
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
colSums(P)
P

### get alpha, the actual Pick 6 track take at Belmont Park
alpha = 0.05 #FIXME
# alpha = 0.2 #FIXME
alpha

# ### get m, the vector m = (m_1,...,m_s) horses in each of the s races
# m = (D %>% group_by(race_num) %>% summarise(num_horses = n()))$num_horses
# m

#############################
### lambda-chalky tickets ###
#############################

lambda_a_chalky_P <- function(lambda,a,P) {
  ### lambda in [0,1] is a tilt parameter measuring how chalky our strategy is
  ### a in 1,...,m is a cutoff parameter indicating which horses we want to tilt upwards or downwards
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### output a probability matrix Q such that Q[i,j] is the probability that we pick horse i to win race j
  Q = matrix(0, nrow=nrow(P), ncol=ncol(P))
  rownames(Q) = rownames(P)
  colnames(Q) = colnames(P)
  Q
  ### m = (m_1,...,m_s), where m_j is the number of horses in race j,
  ### which is the number of nonzero entries in P[,j]
  m = colSums(P != 0)
  s = length(m) # number of horse races
  for (j in 1:s) {
    m_j = unname(m[j])
    if (1 <= a & a < m_j) {
      cutoff = sort(P[,j], decreasing = TRUE)[a]
      ###cutoff = 1/m_j
      longshots = unname(which(P[,j] < cutoff))
      favorites = unname(which(P[,j] >= cutoff))
    } else {
      stop(paste0("a = ", a, " is invalid"))
    }
    # uniform dist
    u = c(rep(1/m_j, m_j), rep(0,length(P[,j])-m_j)) 
    # lambda-a chalky dist
    c_a = rep(0, length(P[,j]))
    c_a[favorites] = P[favorites,j]
    c_a = c_a/sum(c_a) 
    if (0 <= lambda & lambda <= 1/2) {
      Q[,j] = (1-2*lambda)*u + (2*lambda)*P[,j]
    } else if (1/2 <= lambda & lambda <= 1) {
      Q[,j] = (1-2*(lambda-1/2))*P[,j] + (2*(lambda-1/2))*c_a
    } else {
      stop(paste0("lambda = ", lambda, " is invalid"))
    }
  }
  Q
}
# ### check
# lambda_a_chalky_P(lambda=0, a=3, P)
# lambda_a_chalky_P(lambda=0.5, a=3, P)
# all(lambda_a_chalky_P(lambda=0.5, a=3, P) == P)
# lambda_a_chalky_P(lambda=1, a=3, P)
# lambda_a_chalky_P(lambda=0.75, a=3, P)
# colSums(lambda_a_chalky_P(lambda=0.75, a=3, P))
# lambda_a_chalky_P(lambda=1, a=1, P)
# lambda_a_chalky_P(lambda=1, a=6, P)

########################################################
### Expected Profit Procedure, vectorized over n,k,C ###
########################################################

P_tau <- function(tau,P) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### P is a matrix such that P[i,j] is the true probability that horse i wins race j
  s = length(tau) # number of horse races
  temp = 1
  for (j in 1:s) {
    temp = temp * P[tau[j],j]
  }
  temp
}
# ### check
# P_tau(tau=c(4,3,1,3,4,2),P)
# P_tau(tau=c(1,5,7,1,5,8),P)

P_W_neq_0_given_tau <- function(tau,Q,ns) {
  ### tau is a vector of length s (num. races) in which tau[j] is the index of the horse which wins race j
  ### Q is a matrix in which Q[i,j] is the probability that we submit a ticket in which horse i wins in race j
  ### ns is a vector over n, where n is our number of submitted tickets
  s = length(tau) # number of horse races
  temp = 1
  for (j in 1:s) {
    temp = temp * Q[tau[j],j]
  }
  result = 1 - (1 - temp)^ns
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
  temp = ks
  names(temp) = paste0("k=",ks)
  for (j in 1:s) {
    temp = temp * R[tau[j],j]
  }
  temp
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
  
  # browser()
  Emat = as.data.frame(Emat)
  Emat$n = ns
  Emat %>% 
    pivot_longer(-n, values_to="E_W_ratio") %>%
    mutate(k = as.numeric(str_sub(name,3))) %>% 
    select(-name) %>% 
    relocate(k, .after=n)
}
# ### check
# tau = c(4,3,1,3,4,2)
# E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=F)
# E_profit_term_given_tau(tau,P=P,Q=P,ns=10^(1:6),R=P,ks=10^(1:6),matrix_version=T)

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
  df_E_profit = left_join(EWR1, T_mat) %>% mutate(E_profit = t*E_W_ratio-n)
  df_E_profit
}
### check
# E_profit(P,Q=P,R=P,ns=10^(1:6),ks=10^(1:6),Cs=c(0,2500,38000,1e5),alphas=c(0,0.05,0.18)) ### takes abt 1 minute
# E_profit(P,Q=P,R=P,ns=10^(1:6),ks=10^(1:6),Cs=c(0,2500,38000,1e5),alphas=c(0,0.05,0.18)) ### takes abt 1 minute


