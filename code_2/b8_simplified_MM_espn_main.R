
NUM_FOLDS_WPDF_PARALLELIZATION = 100 #FIXME

source("a2_main.R")

### Generalized Poisson Binomial distribution
library(PoissonBinomial)

m = 63 ### number of games in the NCAA tournament, one below a power of 2 
##### p = .773

#################################################
### Expected Max Score of n q-Chalky Brackets ###
#################################################

ns = 10^(0:8)
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

##################
### Plot Grids ###
##################

plot_grid_maxEspnScore_SMM = tibble(expand.grid(
  # p = 0.75,
  p = 0.8,
  q1 = seq(0.5, 1, by=0.1),
  q2 = seq(0.5, 1, by=0.1),
  q3 = seq(0.5, 1, by=0.1),
  q4 = seq(0.5, 1, by=0.1),
  q5 = seq(0.5, 1, by=0.1),
  q6 = seq(0.5, 1, by=0.1)
))
plot_grid_maxEspnScore_SMM


