
################################################################
### Simplified March Madness:
### n submitted Ber(q) brackets vs. 1 true Ber(p) bracket ###
################################################################

source("a2_main.R")

output_folder = "plot_11/"
m = 63 ### number of games in the NCAA tournament, one below a power of 2 

#########################################################
### Compute the CDF of the expected max Hamming score ###
#########################################################

#################################################
### CDF of expected max Hamming score, using Poisson-Binomial dist.

cdf_eMaxHammingScore_given_u_pb <- function(m,u,q) {
  probs = c(rep(1-q,u), rep(q,m-u))
  cdf_eScore_given_u = ppbinom(0:m, probs)
  names(cdf_eScore_given_u) = paste0("a=",0:m)
  cdf_eScore_given_u
}

# cdf_eMaxHammingScore_given_u_pb(m,u=5,q=0.8)
# plot(cdf_eMaxHammingScore_given_u_pb(m,u=5,q=0.8))

#################################################
### CDF of expected max score, using Generalized-Poisson-Binomial dist.

cdf_eMaxHammingScore_given_u_gpb <- function(m,u,q,score_method="Hamming") {
  ### number of rounds R
  R = log2(m+1) 
  ### assume m is a power of 2
  if (R %% 1 != 0) {
    stop("m must be a power of 2")
  }
  
  ### get the weights for the scoring method
  if (score_method == "ESPN") {
    ### wrs is a vector of length R, one value of w for each round, 
    ### where w is the number of points scored in this round
    # wrs = 2^(0:(R-1))
    stop("ESPN score is not supported")
  } else if (score_method == "Hamming") {
    wrs = rep(1,R)
  } else {
    stop("this `score_method` is not supported")
  }
  mrs = 2^(R - 1:R) # number of bits in each round
  names(mrs) = 1:length(mrs)
  prod(mrs+1) ### this many in the for loop...
  
  w_vec = flatten_dbl(sapply(1:R, function(r) rep(wrs[r], mrs[r])))
  zero_vec = rep(0, m)
  u_vec = rep(u, m) ### only because q is constant
  
  probs = c(rep(1-q,u), rep(q,m-u))
  cdf_eScore_given_u = pgpbinom(NULL, probs, val_p=w_vec, val_q=zero_vec, method = "DivideFFT")
  names(cdf_eScore_given_u) = paste0("a=",0:(length(cdf_eScore_given_u)-1))
  cdf_eScore_given_u
}

# cdf_eMaxHammingScore_given_u_gpb(m,u=5,q=0.8,score_method="Hamming")
# plot(cdf_eMaxHammingScore_given_u_gpb(m,u=5,q=0.8,score_method="Hamming"))

#####################################################
### CDF of expected max Hamming score, using combinatorics

h_density <- function(m,u,v) {
  ### m is an integer, the total number of bits
  ### u is an integer, number of bits that are 0 in tau
  ### v is an integer, number of bits that are 0 in x
  a = 0:m
  a = m-a ### to get hamming score rather than hamming dist
  
  u1 = max(u,v)
  d = a - abs(u-v)
  d1 = d/2
  d1_L = 0
  d1_U = min(u,v,m-u1)
  
  d11 = d1[d1_L <= d1 & d1 <= d1_U & d %% 2 == 0]
  vec = choose(u1, abs(u-v)+d11) * choose(m-u1, d11)
  vec2 = numeric(length(d1))
  vec2[d1_L <= d1 & d1 <= d1_U & d %% 2 == 0] = vec
  density_ = vec2 / sum(vec2)
  return(density_)
  # cdf_ = cumsum(vec) / sum(vec)
  # return(cdf_)
}
# h_density(m,25,22)
# sum(h_density(m,25,22))

cdf_eMaxHammingScore_given_u_combinatorics <- function(m,u,q) {
  vals = matrix(nrow=m+1,ncol=m+1)
  rownames(vals) = paste0("v=",0:m)
  colnames(vals) = paste0("a=",0:m)
  for (v in 0:m) {
    pv = dbinom(v, size=m, prob=1-q)
    vals[v+1,] = h_density(m,u,v) * pv
  }
  density_ = colSums(vals)
  cdf_ = cumsum(density_)
  cdf_
}

# cdf_eMaxHammingScore_given_u_combinatorics(m,u=5,q=0.8)
# plot(cdf_eMaxHammingScore_given_u_combinatorics(m,u=5,q=0.8))

#####################################################
### check that the three CDFs are the same

# qs = seq(0.5,1,by=0.5)
# q = 0.8
# cdfas = matrix(nrow=m+1,ncol=m+1)
# cdfbs = matrix(nrow=m+1,ncol=m+1)
# cdfcs = matrix(nrow=m+1,ncol=m+1)
# for (u in 0:m) {
#   cdfa = cdf_eMaxHammingScore_given_u_pb(m,u,q)
#   cdfb = cdf_eMaxHammingScore_given_u_gpb(m,u,q,score_method="Hamming")
#   cdfc = cdf_eMaxHammingScore_given_u_combinatorics(m,u,q)
#   cdfas[u+1,] = cdfa
#   cdfbs[u+1,] = cdfb
#   cdfcs[u+1,] = cdfc
# }
# 
# sum(abs(cdfas - cdfbs)) ### the 2 CDFs are the same if this is nearly 0
# sum(abs(cdfas - cdfcs)) ### the 2 CDFs are the same if this is nearly 0
# 
# j=21
# plot(cdfas[j,])
# plot(cdfbs[j,])
# plot(cdfcs[j,])

##############################################
### Compute the expected max Hamming score ###
##############################################

#####################################################
### Expected Hamming score, using Poisson-Binomial dist. and combinatorics

eMaxHammingScore <- function(m,p,q,method="pb",ns=10^(0:8)) {
  escore = numeric(length(ns))
  names(escore) = paste0("n=",ns)
  for (u in 0:m) {
    
    if (method=="pb") {
      cdf_given_u = cdf_eMaxHammingScore_given_u_pb(m,u,q)
    } else if (method=="gpb") {
      cdf_given_u = cdf_eMaxHammingScore_given_u_gpb(m,u,q,score_method="Hamming")
    } else if (method == "combinatorics") {
      cdf_given_u = cdf_eMaxHammingScore_given_u_combinatorics(m,u,q)
    } else {
      stop("`method` is not supported")
    }
    
    pu = dbinom(u, size=m, prob=1-p)
    for (j in 1:length(ns)) {
      n = ns[j]
      escore[j] = escore[j] + 1 - pu * sum(cdf_given_u^n)
    }
    
  }
  escore
}

### check that the three EScores are the same
# eMaxHammingScore(m,p=0.7,q=0.8,method="pb")
# eMaxHammingScore(m,p=0.7,q=0.8,method="gpb")
# eMaxHammingScore(m,p=0.7,q=0.8,method="combinatorics")

###########################################################
### Compute win probability, for SMM with Hamming score ###
###########################################################

wpMaxHammingScore <- function(m,q,r,p=0.75,method="pb",ns=10^(0:8),ks=10^(0:8)) {
  wp = matrix(0, nrow=length(ns), ncol=length(ks))
  rownames(wp) = paste0("n=",ns)
  colnames(wp) = paste0("k=",ks)
  for (u in 0:m) {
    
    if (method=="pb") {
      cdf_x_given_u = cdf_eMaxHammingScore_given_u_pb(m,u,q)
      cdf_y_given_u = cdf_eMaxHammingScore_given_u_pb(m,u,r)
    } else if (method=="gpb") {
      cdf_x_given_u = cdf_eMaxHammingScore_given_u_gpb(m,u,q,score_method="Hamming")
      cdf_y_given_u = cdf_eMaxHammingScore_given_u_gpb(m,u,r,score_method="Hamming")
    } else if (method == "combinatorics") {
      cdf_x_given_u = cdf_eMaxHammingScore_given_u_combinatorics(m,u,q)
      cdf_y_given_u = cdf_eMaxHammingScore_given_u_combinatorics(m,u,r)
    } else {
      stop("`method` is not supported")
    }
    
    pu = dbinom(u, size=m, prob=1-p)
    for (j in 1:length(ns)) {
      for (l in 1:length(ks)) {
        n = ns[j]
        k = ks[l]
        wp[j,l] = wp[j,l] + pu * sum( cdf_y_given_u^k * (cdf_x_given_u^n - lag(cdf_x_given_u, default=0)^n ) )
      }
    }
    
  }
  wp
}

# wpMaxHammingScore(m,q=0.7,r=0.7)

######################################################
### Compute the CDF of the expected max ESPN score ###
### using the Generalized Poisson Binomial dist.   ###
######################################################

cdf_eMaxWeightedScoreByRound_given_u_gpb <- function(mrs,wrs,urs,qrs) {
  ### compute the CDF of a Generalized Poisson Binomial distribution
  R = length(mrs)
  w_vec = flatten_dbl(sapply(1:R, function(r) rep(wrs[r], mrs[r])))
  zero_vec = rep(0, m)
  probs = flatten_dbl(sapply(1:R, function(r) c(rep(1-qrs[r], urs[r]),  rep(qrs[r], mrs[r]-urs[r]))  ))
  cdf_eScore_given_u = pgpbinom(NULL, probs, val_p=w_vec, val_q=zero_vec, method = "DivideFFT")
  names(cdf_eScore_given_u) = paste0("a=",0:(length(cdf_eScore_given_u)-1))
  cdf_eScore_given_u
}

# ### checks 
# x1 = cdf_eMaxWeightedScoreByRound_given_u_gpb(mrs=c(32,16,8,4,2,1), wrs=rep(1,6),
#                                               urs=c(16,8,4,2,1,0), qrs=rep(0.7,6))
# x2 = cdf_eMaxHammingScore_given_u_gpb(m, u=31, q=0.7, score_method="Hamming")
# plot(x1)
# plot(x2)
# sum(abs(x1-x2)) ### should be zero
# x3 = cdf_eMaxWeightedScoreByRound_given_u_gpb(mrs=c(32,16,8,4,2,1), wrs=c(1,2,4,8,16,31),
#                                               urs=c(16,8,4,2,1,0), qrs=rep(0.7,6))
# x4 = cdf_eMaxWeightedScoreByRound_given_u_gpb(mrs=c(32,16,8,4,2,1), wrs=c(1,2,4,8,16,31),
#                                               urs=c(16,8,4,2,1,0), qrs=c(0.5,0.6,0.7,0.7,0.8,0.9))
# plot(x3)
# plot(x4)

cdf_eMaxWeightedScoreByRound_given_u_gpb <- function(mrs,w_vec,urs,qrs) {
  ### compute the CDF of a Generalized Poisson Binomial distribution
  probs = flatten_dbl(sapply(1:length(mrs), function(r) c(rep(1-qrs[r], urs[r]),  rep(qrs[r], mrs[r]-urs[r]))  ))
  pgpbinom(NULL, probs, val_p=w_vec, val_q=rep(0, m), method = "DivideFFT")
}

###########################################
### Compute the expected max ESPN score ###
###########################################

eMaxWeightedScoreByRound_gpb <- function(m,prs,qrs,ns=10^(0:8),score_method="ESPN",print_every_n=2500) {
  ### number of rounds R
  R = log2(m+1) 
  ### assume m is a power of 2
  if (R %% 1 != 0) {
    stop("m must be a power of 2")
  }
  ### number of bits (matches) in each round
  mrs = 2^(R - 1:R)
  
  ### prs = (p1,...,pR) must be a vector of length R, representing the submitted chalkiness in round r
  ### qrs = (q1,...,qR) must be a vector of length R, representing the true chalkiness in round r
  if (length(qrs) != R | length(prs) != R ) {
    stop("`prs` and `qrs` must have length R")
  }
  
  ### get the GPB-weights for the scoring method
  if (score_method == "ESPN") {
    ### wrs is a vector of length R, one value of w for each round,
    ### where w is the number of points scored in this round
    wrs = 2^(1:R - 1)
  } else if (score_method == "Hamming") {
    wrs = rep(1,R)
  } else {
    stop("this `score_method` is not supported")
  }
  w_vec = flatten_dbl(sapply(1:R, function(r) rep(wrs[r], mrs[r])))
  
  ### get all possible combinations of urs
  mrs_lst = list()
  for (r in 1:R) {
    mrs_lst[[r]] = 0:mrs[r]
  }
  urs_vec = expand.grid(mrs_lst)
  
  # browser()
  ### iterate over all possible combinations of urs
  ### this is the main for loop, which is very time consuming!
  num_a = length(
    cdf_eMaxWeightedScoreByRound_given_u_gpb(
      mrs,w_vec,as.numeric(urs_vec[1,]),qrs)
  ) # max possible weighted score
  pus = numeric(nrow(urs_vec))
  cdfs_u = matrix(nrow=nrow(urs_vec), ncol=num_a)
  for (i in 1:nrow(urs_vec)) {
  # for (i in 1:1500) {
    if (i %% print_every_n == 0) print(paste0(i,"/",nrow(urs_vec)))
    
    urs = as.numeric(urs_vec[i,]) ### vector (u1,...,uR)
    pu = prod(sapply(1:R, function(r) dbinom(urs[r], size=mrs[r], prob=1-prs[r]) )) ### assuming constant p for each "true" bit
    # ### compute the CDF of a Generalized Poisson Binomial distribution
    # cdf_given_u = cdf_eMaxWeightedScoreByRound_given_u_gpb(mrs,wrs,urs,qrs)
    cdf_given_u = cdf_eMaxWeightedScoreByRound_given_u_gpb(mrs,w_vec,urs,qrs)
    pus[i] = pu
    cdfs_u[i,] = cdf_given_u
  }
  
  escore = rep(num_a, length(ns))
  names(escore) = paste0("n=",ns)
  for (j in 1:length(ns)) {
    n = ns[j]
    escore[j] = escore[j] - sum(pus * rowSums(cdfs_u)^n)
  }
  escore
}

# library(compiler)
# eMaxWeightedScoreByRound_gpb_Compiled <- cmpfun(eMaxWeightedScoreByRound_gpb)

# library(doParallel)
# registerDoParallel(cores=12)
# system.time(loopParallelD <- foreach(i=1:dim(m)[1], .combine=c) %dopar% mean(m[i,]))



# ptm <- proc.time() # Start the clock!
# # eMaxWeightedScoreByRound_gpb_Compiled(m, prs=rep(0.7,6), qrs=rep(0.7,6), print_every_n=1)
# eMaxWeightedScoreByRound_gpb(m, prs=rep(0.7,6), qrs=rep(0.7,6), print_every_n=1)
# ptm1 = proc.time() - ptm # Stop the clock
# ptm1
# ### about 8 minutes to do just one of these
# ### check that the three EScores are the same
# eMaxWeightedScoreByRound_gpb(m, prs=rep(0.7,6), qrs=rep(0.7,6), print_every_n=1, score_method = "Hamming")




# eMaxHammingScore(m,p=0.7,q=0.8,method="pb")
# eMaxHammingScore(m,p=0.7,q=0.8,method="gpb")
# eMaxHammingScore(m,p=0.7,q=0.8,method="combinatorics")


