
#####################################################
### Load real horse racing data from Belmont Park ###
#####################################################

source("../a0_loadStuff.R")
output_folder = "plots/"

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

### get m, the vector m = (m_1,...,m_s) horses in each of the s races
m = (D %>% group_by(race_num) %>% summarise(num_horses = n()))$num_horses
m

### get alpha, the actual Pick 6 track take at Belmont Park
# alpha = 0.05 #FIXME
alpha = 0.2 #FIXME
alpha

#############################
### opponent's strategy R ###
#############################

R_opp_tilted <- function(lambda,P,m) {
  ### lambda in [0,1] is a tilt parameter measuring how chalky the opponents' strategy is
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### m is the vector m = (m_1,...,m_s) horses in each of the s races
  ### output R, a probability matrix such that R[i,j] is the probability that an opponent picks horse i to win race j
  R = matrix(0, nrow=nrow(P), ncol=ncol(P))
  rownames(R) = rownames(P)
  colnames(R) = colnames(P)
  R
  s = length(m) # number of horse races
  for (j in 1:s) {
    m_j = m[j]
    longshots = unname(which(P[,j] < 1/m_j))
    favorites = unname(which(P[,j] >= 1/m_j))
    for (i in longshots) {
      R[i,j] = (1-lambda)*P[i,j]
    }
    G_j = sum(P[longshots,j] - R[longshots,j])
    for (i in favorites) {
      R[i,j] = P[i,j] + G_j * P[i,j]/sum(P[favorites,j])
    }
  }
  R
}

# ### check
# colSums(R_opp_tilted(lambda=0,P,m))
# R_opp_tilted(lambda=0,P,m)
# P
# colSums(R_opp_tilted(lambda=0.5,P,m))
# R_opp_tilted(lambda=0.5,P,m)
# P
# colSums(R_opp_tilted(lambda=1,P,m))
# R_opp_tilted(lambda=1,P,m)
# P

### an example
R0 = R_opp_tilted(lambda=0,P,m)
Rhalf = R_opp_tilted(lambda=0.5,P,m)
R1 = R_opp_tilted(lambda=1,P,m)

###############################################################################################
### parameters n,k,C which we will set for now, but for which we will try many diff. values ###
###############################################################################################

n = 100
# k = 25000
k = 1e6
C = 38000

#################################
### Expected Profit Procedure ###
#################################

E_W_opp <- function(P,R,k,m) {
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### R is a probability matrix such that R[i,j] is the probability that an opponent picks horse i to win race j
  ### k is the number of tickets submitted by opponents
  ### m is the vector m = (m_1,...,m_s) horses in each of the s races
  ### output the expected number of winning tickets by opponents
  result = k
  s = length(m) # number of horse races
  for (j in 1:s) {
    result_j = 0
    m_j = m[j]
    for (i in 1:m_j) {
      result_j = result_j + R[i,j]*P[i,j]
    }
    result = result*result_j
  }
  result
}
### check
E_W_opp(P,R0,k,m)
E_W_opp(P,Rhalf,k,m)
E_W_opp(P,R1,k,m)
E_W_opp_ = E_W_opp(P,R1,k,m)
E_W_opp_

# P = matrix(c(1/3,1/3,1/3,1/2,1/4,1/4,1/2,1/3,1/6), nrow=3,byrow=T)
# P
# R = matrix(c(1,0,0,3/4,0,1/4,1/2,1/2,0), nrow=3,byrow=T)
# R
# m = c(3,3,3)
# E_W_opp(P,R,k=100,m) 
# E_W_opp(P,R,k=1000,m)

project_simplex <- function(q_vec, stopThr=1e-5, ballRadius=1) {
  ### q_vec is a probability vector of length m
  paramMu <- min(q_vec) - ballRadius
  objFun <- sum(pmax(q_vec - paramMu, 0)) - ballRadius
  
  while(abs(objFun) > stopThr) {
    objFun <- sum(pmax(q_vec - paramMu, 0)) - ballRadius
    df <- sum(-((q_vec - paramMu) > 0))
    paramMu <- paramMu - (objFun / df)
  }
  
  vX <- pmax(q_vec - paramMu, 0)
  return(vX)
}
# project_simplex(c(1/2,1/3,1/6))
# project_simplex(c(2,1/3,1/6))
# project_simplex(c(1,1/3,1/6))
# c(1,1/3,1/6)/sum(c(1,1/3,1/6))
# project_simplex(c(1,1/3,0))
# project_simplex(c(2/3,1/3,0))
# project_simplex(c(1,1/3,-1/3))
# project_simplex(c(1/2,1/3,-1/3))

q_star_as_argmin <- function(p_vec,n) {
  ### n is our number of submitted tickets
  ### p_vec is a probability vector of length m
  ### output: q_vec_star is a probability vector
  m = length(p_vec) # the number of horses in this horse race
  
  f <- function(q_vec) {
    ### q_vec is a probability vector of length m
    sum(p_vec*(1-q_vec)^n)
  }
  f_grad <- function(q_vec) {
    -n*p_vec*(1-q_vec)^(n-1)
  }
  
  ### how to minimize a nonlinear convex objective function on the probability simplex:
  ### Accelerated Projected Gradient Descent
  ### All you need is to calculate the Gradient of the function f and project each iteration onto the Unit Simplex
  ### https://math.stackexchange.com/questions/88746/convex-minimization-over-the-unit-simplex
  stepSize = 0.01
  simplexRadius = 1
  stopThr = 1e-5
  epsilon = 1e-8
  q_vec = rep(1/m, m)
  # for (ii in 1:numIterations) {
  while (TRUE) {
    fG <- f_grad(q_vec) # the gradient of f
    q_vec_1 <- q_vec - (stepSize * fG) # gradient descent update
    q_vec_1 <- project_simplex(q_vec_1, stopThr, simplexRadius) 
    conv <- abs(q_vec - q_vec_1) 
    conv1 <- sum(conv) # convergence
    q_vec <- q_vec_1
    if (conv1 < epsilon) { break }
  }
  q_vec
}
### check
# m = 7
# n = 10
# p_vec = c(16,10,6,3,2,1,1)
# p_vec = p_vec/sum(p_vec)
# q_star_as_argmin(m=7, n=10, p_vec=p_vec)
# sum(q_star_as_argmin(m=7, n=10, p_vec=p_vec))
# f(rep(1/m,m))
# f(p_vec)
# f(q_star_as_argmin(m=7, n=10, p_vec=p_vec))

Q_star_as_argmin <- function(P,n) {
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### n is our number of submitted tickets
  ### output: Q_star is a probability matrix such that Q_star[i,j] is the probability that we pick horse i to win race j
  s = length(m) # number of horse races
  Q_star = matrix(nrow=nrow(P), ncol=ncol(P))
  for (j in 1:s) {
    print(paste0("Q_star_as_argmin j=",j))
    q_star_j = q_star_as_argmin(P[,j],n)
    Q_star[,j] = q_star_j
  }
  Q_star
}
### check
Q_star = Q_star_as_argmin(P,n)
Q_star_

# P = matrix(c(1/3,1/3,1/3,1/2,1/4,1/4,1/2,1/3,1/6), nrow=3,byrow=T)
# P
# m = c(3,3,3)
# Q_star_as_argmin(P,n=100,m)
# Q_star_as_argmin(P,n=10,m)
# Q_star_as_argmin(P,n=3,m)

P_W0 <- function(P,n,m,Q_star) {
  ### P is a probability matrix such that P[i,j] is the true probability that horse i wins race j
  ### n is our number of submitted tickets
  ### m is the vector m = (m_1,...,m_s) horses in each of the s races
  ### Q_star is a probability matrix such that Q_star[i,j] is the probability that we pick horse i to win race j
  ### output the probability that we have 0 winning tickets
  result = 1
  s = length(m) # number of horse races
  for (j in 1:s) {
    result_j = 0
    m_j = m[j]
    for (i in 1:m_j) {
      result_j = result_j + (1-Q_star[i,j])^n*P[i,j]
    }
    result = result*result_j
  }
  result
}
### check
P_W0_ = P_W0(P,n,m,Q_star_)
P_W0_

# P = matrix(c(1/3,1/3,1/3,1/2,1/4,1/4,1/2,1/3,1/6), nrow=3,byrow=T)
# P
# m = c(3,3,3)
# n = 1
# Q_star = Q_star_as_argmin(P,n,m)
# P_W0(P,n,m,Q_star)

total_prize_money <- function(n,k,C,alpha) {
  ### n is our number of submitted tickets
  ### k is the number of tickets submitted by opponents
  ### C is the carryover
  ### alpha is the track take
  ### output the total prize money
  (n+k+C)*(1-alpha)
}
### check
T_ = total_prize_money(n,k,C,alpha)
T_

E_profit <- function(n, T_, P_W0_, E_W_opp_) {
  ### n is our number of submitted tickets
  ### T_ is the total prize money
  ### P_W0_ is the probability we have zero submitted brackets
  ### E_W_opp_ is the expected number of our opponents' winning tickets
  ### output (the lower bound of) our expected profit
  (1-P_W0_)*(T_/(1+E_W_opp_)) - n
}
### check
E_profit(n,T_,P_W0_,E_W_opp_)

# T_ = total_prize_money(n=100,k=10000,C=38000,alpha=0.04)
# T_
# E_profit(n=100,T_,P_W0_=0.95,E_W_opp_=3)
# E_profit(n=100,T_,P_W0_=0.9,E_W_opp_=3)
# E_profit(n=100,T_,P_W0_=0.9,E_W_opp_=10)


