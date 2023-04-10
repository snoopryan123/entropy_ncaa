
source("a2_main.R")

### Generalized Poisson Binomial distribution
library(PoissonBinomial)

m = 63 ### number of games in the NCAA tournament, one below a power of 2 
##### p = .773
ns = 10^(0:8) ### vector of possible values of n and k

#################################################
### Expected Max Score of n q-Chalky Brackets ###
#################################################

eMaxEspnScore_SMM <- function(m,p,qrs,score="ESPN",print_num0="",print_num1="",print_every_n=2500) {
  ### m is the total number of bits in the tournament, and is a power of 2
  ### p is the "true" bit probability, which we assume is constant throughout the tournament
  ### R = log2(m+1) is the number of rounds in the tournament
  ### qrs is a vector of length R, one value of q for each round, where q is our submitted bit prob.
  ### return a vector of Expected_Max_Score for various values of n (num submitted brackets)
  R = log2(m+1) # number of rounds
  ### assume m is a power of 2
  if (R %% 1 != 0) {
    stop("m must be a power of 2")
  }
  ### assume the same q in each round r
  if (length(qrs) != R) {
    stop("qrs must have R values of q")
  }
  ### get the weights for the scoring method
  if (score == "ESPN") {
    ### wrs is a vector of length R, one value of w for each round, 
    ### where w is the number of points scored in this round
    wrs = 2^(1:R-1)
  } else if (score == "Hamming") {
    wrs = rep(1,R)
  } else {
    stop("this scoring method hasn't been implemented")
  }
  mrs = 2^(R - 1:R) # number of bits in each round
  names(mrs) = 1:length(mrs)
  prod(mrs+1) ### this many in the for loop...
  
  ### get all possible combinations of u_vec
  mrs_lst = list()
  for (r in 1:R) {
    mrs_lst[[r]] = 0:mrs[r]
  }
  u_vecs = expand.grid(mrs_lst)
  w_vec = flatten_dbl(sapply(1:R, function(r) rep(wrs[r], mrs[r])))
  zero_vec = rep(0, m)
  
  ### iterate over all possible combinations of u_vec
  ncol_ = length(pgpbinom(NULL, rep(0.5,m), w_vec, zero_vec, method = "DivideFFT")) ### number of possible ESPN scores
  # cdf_mat = matrix(nrow = nrow(u_vecs), ncol = ncol_)
  cdf_array = array(dim = c(nrow(u_vecs), ncol_, length(ns)))
  # for (i in 1:1000) {
  for (i in 1:nrow(u_vecs)) {
    if (i %% print_every_n == 0) print(paste0("GRID idx ",print_num0," of ",print_num1,"; cdf_maxEspnScore_SMM iter i=",i," of ",nrow(u_vecs)))
    
    urs = u_vecs[i,] ### vector (u1,...,uR)
    u = sum(urs)
    
    probs = flatten_dbl(sapply(1:R, function(r) c(rep(1-qrs[r], urs[r]),  rep(qrs[r], mrs[r]-urs[r]))  ))
    cdf_eScore_given_u = pgpbinom(NULL, probs, val_p=w_vec, val_q=zero_vec, method = "DivideFFT")
    pu = prod(sapply(1:R, function(r) dbinom(urs[1,r], size=mrs[r], prob=p) )) ### assuming constant p for each "true" bit
    # cdf_mat[i,] = pu * cdf_eScore_given_u^ns
    
    for (j in 1:length(ns)) {
      n = ns[j]
      cdf_array[i,,j] = pu * cdf_eScore_given_u^n
    }
    
    rm(urs,u,probs,cdf_eScore_given_u,pu,n) ### for HPCC using less memory
  }
  
  # escore = sum(1 - colSums(cdf_mat, na.rm=T))
  # escore*10 ### multiply by 10 to get the real ESPN score
  escore = 1 - apply(cdf_array, MARGIN=c(2,3), FUN=sum)
  escore = colSums(escore)*10 ### multiply by 10 to get the real ESPN score
  names(escore) = paste0("n=",ns)
  rm(R,wrs,mrs,mrs_lst,u_vecs,w_vec,zero_vec,ncol_,cdf_array) ### for HPCC using less memory
  gc(reset=TRUE) ### garbage collector, to free up memory for HPCC
  return(escore)
}

# eMaxEspnScore_SMM(m, p=0.75, qrs=c(0.6,0.65,0.7,0.75,0.8,0.9), score="ESPN") ### takes ~5 minutes

######################################################################
### Win Probability of n q-Chalky Brackets vs. k r-Chalky Brackets ###
######################################################################

wpEspnScore_SMM <- function(m,p,qrs,rrs,score="ESPN",print_num0="",print_num1="",print_every_n=10000) {
  ### m is the total number of bits in the tournament, and is a power of 2
  ### p is the "true" bit probability, which we assume is constant throughout the tournament
  ### R = log2(m+1) is the number of rounds in the tournament
  ### qrs is a vector of length R, one value of q for each round, where q is our submitted bit prob.
  ### return a vector of Expected_Max_Score for various values of n (num submitted brackets)
  R = log2(m+1) # number of rounds
  ### assume m is a power of 2
  if (R %% 1 != 0) {
    stop("m must be a power of 2")
  }
  ### assume the same q in each round r
  if (length(qrs) != R) {
    stop("qrs must have R values of q")
  }
  ### get the weights for the scoring method
  if (score == "ESPN") {
    ### wrs is a vector of length R, one value of w for each round, 
    ### where w is the number of points scored in this round
    wrs = 2^(1:R-1)
  } else if (score == "Hamming") {
    wrs = rep(1,R)
  } else {
    stop("this scoring method hasn't been implemented")
  }
  mrs = 2^(R - 1:R) # number of bits in each round
  names(mrs) = 1:length(mrs)
  prod(mrs+1) ### this many in the for loop...
  
  ### get all possible combinations of u_vec
  mrs_lst = list()
  for (r in 1:R) {
    mrs_lst[[r]] = 0:mrs[r]
  }
  u_vecs = expand.grid(mrs_lst)
  w_vec = flatten_dbl(sapply(1:R, function(r) rep(wrs[r], mrs[r])))
  zero_vec = rep(0, m)
  
  ### iterate over all possible combinations of u_vec
  # ncol_ = length(pgpbinom(NULL, rep(0.5,m), w_vec, zero_vec, method = "DivideFFT")) ### number of possible ESPN scores
  # cdf_array = array(dim = c(nrow(u_vecs), ncol_, length(ns)))
  prob_mat = matrix(0, nrow=length(ns), ncol=length(ns))
  rownames(prob_mat) = paste0("n=",ns)
  colnames(prob_mat) = paste0("k=",ns)
  # for (i in 1:1000) {
  for (i in 1:nrow(u_vecs)) {
    if (i %% print_every_n == 0) print(paste0("GRID idx ",print_num0," of ",print_num1,"; cdf_maxEspnScore_SMM iter i=",i," of ",nrow(u_vecs)))
    
    urs = u_vecs[i,] ### vector (u1,...,uR)

    ##### P( f(x|tau) < f(y|tau) | u )
    probs_x = flatten_dbl(sapply(1:R, function(rd) c(rep(1-qrs[rd], urs[rd]),  rep(qrs[rd], mrs[rd]-urs[rd]))  ))
    probs_y = flatten_dbl(sapply(1:R, function(rd) c(rep(1-rrs[rd], urs[rd]),  rep(rrs[rd], mrs[rd]-urs[rd]))  ))
    density_score_y_given_u = dgpbinom(NULL, probs_y, val_p=w_vec, val_q=zero_vec, method = "DivideFFT")
    cdf_score_x_given_u     = pgpbinom(NULL, probs_x, val_p=w_vec, val_q=zero_vec, method = "DivideFFT")
    cdf_score_x_given_u_PlusOne = lead(cdf_score_x_given_u)
    cdf_score_x_given_u_PlusOne[length(cdf_score_x_given_u_PlusOne)] = 1
    p_scoreX_lt_scoreY_given_u = sum(cdf_score_x_given_u_PlusOne * density_score_y_given_u)
    # ### checks
    # tibble(cdf_score_x_given_u, cdf_score_x_given_u_PlusOne)
    # tail(tibble(cdf_score_x_given_u, cdf_score_x_given_u_PlusOne))

    ##### sum_u P( max_j f(xj|tau) >= max_l f(yl|tau) | u) • P(u)
    pu = prod(sapply(1:R, function(r) dbinom(urs[1,r], size=mrs[r], prob=p) )) ### assuming constant p for each "true" bit

    for (j1 in 1:length(ns)) {
      for (j2 in 1:length(ns)) {
        n = ns[j1]
        k = ns[j2]
        p_maxScoreX_geq_maxScoreY_given_u = (1 - p_scoreX_lt_scoreY_given_u^n)^k
        p_nk = pu * p_maxScoreX_geq_maxScoreY_given_u 
        prob_mat[j1,j2] = prob_mat[j1,j2] + p_nk
      }
    }
    rm(urs,u,probs_x,probs_y,density_score_y_given_u,cdf_score_x_given_u,cdf_score_x_given_u_PlusOne,
       p_scoreX_lt_scoreY_given_u,p_maxScoreX_geq_maxScoreY_given_u,pu,n,k) ### for HPCC using less memory
  }
  rm(R,wrs,mrs,mrs_lst,u_vecs,w_vec,zero_vec) ### for HPCC using less memory
  gc(reset=TRUE) ### garbage collector, to free up memory for HPCC
  return(prob_mat)
}

# wpEspnScore_SMM(m, p=0.75, qrs=c(0.6,0.65,0.7,0.75,0.8,0.9), rrs = c(0.6,0.65,0.7,0.75,0.8,0.9), score="ESPN") ### takes ~5 minutes

##################
### Plot Grids ###
##################

plot_grid_maxEspnScore_SMM_v1 = tibble(expand.grid(
  p = seq(0.5, 1, by=0.1),
  q = seq(0, 1, by=0.1),
  score = c("Hamming", "ESPN")
  )) %>%
  mutate(
    q1=q,q2=q,q3=q,q4=q,q5=q,q6=q
  ) %>%
  select(-c(q))
plot_grid_maxEspnScore_SMM_v1


plot_grid_maxEspnScore_SMM_v2 = tibble(expand.grid(
    # p = 0.80,
    # qE = seq(0.5, 1, by=0.1),
    # qL = seq(0.5, 1, by=0.1)
    p = 0.75,
    qE = seq(0.5, 1, by=0.05),
    qL = seq(0.5, 1, by=0.05),
    score = c("ESPN")
    # score = c("Hamming", "ESPN")
  )) %>%
  mutate(
    q1 = qE, q2 = qE, q3 = qE,
    # q4 = qE,
    q4 = qL,
    q5 = qL, q6 = qL
  ) %>%
  select(-c(qE,qL))
plot_grid_maxEspnScore_SMM_v2


plot_grid_wpEspnScore_SMM = tibble(expand.grid(
  p = 0.75,
  qE = seq(0.5, 1, by=0.1),
  qL = seq(0.5, 1, by=0.1),
  rE = seq(0.5, 1, by=0.1),
  rL = seq(0.5, 1, by=0.1)
  # p = 0.75,
  # qE = seq(0.5, 1, by=0.05),
  # qL = seq(0.5, 1, by=0.05),
  # rE = seq(0.5, 1, by=0.05),
  # rL = seq(0.5, 1, by=0.05)
)) %>%
  mutate(
    q1 = qE, q2 = qE, q3 = qE,
    q4 = qL,
    q5 = qL, q6 = qL,
    r1 = rE, r2 = rE, r3 = rE,
    r4 = rL,
    r5 = rL, r6 = rL
  ) %>%
  select(-c(qE,qL,rE,rL))
plot_grid_wpEspnScore_SMM


