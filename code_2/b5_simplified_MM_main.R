
NUM_FOLDS_WPDF_PARALLELIZATION = 50 #FIXME

###################################
### Code for Simplied Theorem 1 ###
###################################

source("a2_main.R")

###############
### Entropy ###
###############

# bracket_set = sample_n_brackets(n=1000000, keep_probs = TRUE)
# entropies = compute_entropies(bracket_set)
# 
# mean(entropies)
# H_ = 48.7
# H_/63
# 
# entropy_Ber_p = function(p) { -p*log(p,base=2) - (1-p)*log(1-p,base=2) }
# entropy_Ber_p
# 
# p = uniroot(function(p) entropy_Ber_p(p) - H_/63, lower=0.5, upper=0.999)$root
# 
# H_/63
# entropy_Ber_p(p)
# p

p = .773
m = 63 ### number of games in the NCAA tournament

############################################
### Expected Score of One Random Bracket ###
############################################

emaxscore_random1 <- function(p) {
  m*(p^2 + (1-p)^2)
}
### check
# emaxscore_random1(0.5)
# emaxscore_random1(0.75)
# emaxscore_random1(0.9)
# emaxscore_random1(1)

############################################
### Expected Score of One Chalky Bracket ###
############################################

emaxscore_chalky1 <- function(p,u) {
  (63-u)*p + u*(1-p)
}
### check
# emaxscore_chalky1(0.5, 3)
# emaxscore_chalky1(0.5, 4)
# emaxscore_chalky1(0.75, 3)
# emaxscore_chalky1(0.75, 4)
# emaxscore_chalky1(0.9, 3)
# emaxscore_chalky1(0.9, 4)

emaxscore_chalky1_umax <- function(p,u_max=4) {
  vec = numeric(u_max+1)
  for (u in 0:u_max) {
    vec[u+1] = emaxscore_chalky1(p,u) * dbinom(u, size=m, prob=1-p)/pbinom(u_max, size=m, prob=1-p, lower.tail = TRUE)
  }
  sum(vec)
}
### check
# emaxscore_chalky1_umax(0.5, 3)
# emaxscore_chalky1_umax(0.5, 4)
# emaxscore_chalky1_umax(0.75, 3)
# emaxscore_chalky1_umax(0.75, 4)
# emaxscore_chalky1_umax(0.9, 3)
# emaxscore_chalky1_umax(0.9, 4)

###############################################
### Expected Max Score of n Random Brackets ###
###############################################

##### Poisson Binomial distribution from package `poibin`
##### make sure not to use the glitchy AF package `poisbinom`
##### https://www.sciencedirect.com/science/article/abs/pii/S0167947312003568

# source("a2_main.R"); m=63; p=.77

cdf_Mn_random_given_u <- function(p,n,u,k) {
  pb_probs = c(rep(p, m-u), rep(1-p, u)) # binomial probabilities
  poibin::ppoibin(k, pp=pb_probs)^n # poisson-binomial distribution 
}
### check
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=45)
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=47)
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=50)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=45)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=47)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=50)

cdf_Mn_random <- function(p,n,k) {
  vec = numeric(m+1)
  for (u in 0:m) {
    vec[u+1] = cdf_Mn_random_given_u(p,n,u,k) * dbinom(u, size=m, prob=1-p)
  }
  sum(vec)
}
### check
# cdf_Mn_random(p=p, n=10, k=45)
# cdf_Mn_random(p=p, n=10, k=47)
# cdf_Mn_random(p=p, n=10, k=50)
# cdf_Mn_random(p=p, n=100, k=45)
# cdf_Mn_random(p=p, n=100, k=47)
# cdf_Mn_random(p=p, n=100, k=50)

escore_Mn_random <- function(p,n) {
  vec = numeric(m+1)
  for (k in 0:m) {
    vec[k+1] = 1 - cdf_Mn_random(p,n,k)
  }
  sum(vec)
}
### check
# escore_Mn_random(p=p, n=10)
# escore_Mn_random(p=p, n=100)
# escore_Mn_random(p=p, n=1000)
# escore_Mn_random(p=p, n=10000)
# escore_Mn_random(p=p, n=100000)
# escore_Mn_random(p=0.8, n=100)
# escore_Mn_random(p=0.9, n=100)
# escore_Mn_random(p=1, n=100)

#################################################
### Expected Max Score of n U-Chalky Brackets ###
#################################################

h <- function(a,m,u,v) {
  ### a is a vector of integers; return(sum(h(a)))
  ### m is an integer, the total number of bits
  ### u is an integer, number of bits that are 0 in tau
  ### v is an integer, number of bits that are 0 in x
  u1 = max(u,v)
  d = a - abs(u-v)
  d1 = d/2
  d1_L = 0
  d1_U = min(u,v,m-u1)
  
  d11 = d1[d1_L <= d1 & d1 <= d1_U & d %% 2 == 0]
  vec = choose(u1, abs(u-v)+d11) * choose(m-u1, d11)
  return(sum(vec))
}
# ### check
# h(a=0,m=12,u=8,v=5)
# h(a=1,m=12,u=8,v=5)
# h(a=2,m=12,u=8,v=5)
# h(a=3,m=12,u=8,v=5)
# choose(8,3)
# h(a=4,m=12,u=8,v=5)
# h(a=5,m=12,u=8,v=5)
# choose(8,4)*choose(4,1)
# h(a=6,m=12,u=8,v=5)
# h(a=7,m=12,u=8,v=5)
# choose(8,5)*choose(4,2)
# h(a=8,m=12,u=8,v=5)
# h(a=9,m=12,u=8,v=5)
# choose(8,6)*choose(4,3)
# h(a=10,m=12,u=8,v=5)
# h(a=11,m=12,u=8,v=5)
# choose(8,7)*choose(4,4)
# h(a=12,m=12,u=8,v=5)
# h(a=13,m=12,u=8,v=5)
# h(a=14,m=12,u=8,v=5)
# h(a=0:12,m=12,u=8,v=5)
# h(a=3,m=12,u=8,v=5)+h(a=5,m=12,u=8,v=5)+h(a=7,m=12,u=8,v=5)+h(a=9,m=12,u=8,v=5)+h(a=11,m=12,u=8,v=5)
  
cdf_Mn_chalky_given_u <- function(p,n,u,k,u_max=10) {
  vec = numeric(u_max+1)
  for (v in 0:u_max) {
    # browser()
    t1 = h(a = ((m-k):m) , m,u,v)
    t2 = h(a = 0:m , m,u,v)
    t3 = dbinom(v, size=m, prob=1-p)/pbinom(u_max, size=m, prob=1-p, lower.tail = TRUE)
    vec[v+1] = t1/t2 * t3
  }
  sum(vec)^n
}
### check
# cdf_Mn_chalky_given_u(p=p, n=5, u=10, k=50, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=5, u=10, k=51, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=50, u=10, k=50, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=50, u=10, k=51, u_max=4)

cdf_Mn_chalky <- function(p,n,k,u_max=10) {
  vec = numeric(m+1)
  for (u in 0:m) {
    vec[u+1] = cdf_Mn_chalky_given_u(p,n,u,k,u_max=u_max) * dbinom(u, size=m, prob=1-p)
  }
  sum(vec)
}
### check
# cdf_Mn_chalky(p=p, n=10, k=25)
# cdf_Mn_chalky(p=p, n=10, k=28)
# cdf_Mn_chalky(p=p, n=100, k=25)
# cdf_Mn_chalky(p=p, n=100, k=28)

escore_Mn_chalky <- function(p,n,u_max=10) {
  print(paste0("escore_Mn_chalky, ", "p=", p, ", n=", n, ", u_max=", u_max))
  vec = numeric(m+1)
  for (k in 0:m) {
    vec[k+1] = 1 - cdf_Mn_chalky(p,n,k,u_max=u_max)
  }
  sum(vec)
}
### check
# escore_Mn_chalky(p=0.5, n=10)
# escore_Mn_chalky(p=0.6, n=10)
# escore_Mn_chalky(p=0.7, n=10)
# escore_Mn_chalky(p=0.7, n=100)

#################################################
### Expected Max Score of n q-Chalky Brackets ###
#################################################

cdfScore_q_chalky_given_uv <- function(m,k,u,v) {
  # print("a1")
  # browser()
  t1 = h(a = ((m-k):m) , m,u,v)
  t2 = h(a = 0:m , m,u,v)
  result = t1 / t2
  result
}
### check
# cdfScore_q_chalky_given_uv(m=m,k=30,u=20,v=20)
# cdfScore_q_chalky_given_uv(m=m,k=30,u=20,v=40)

### precompute cdfScore_q_chalky_given_uv(m,k,u,v)
filename = paste0("plot_thm1/df_cdfScore_q_chalky_given_uv_array_m",m,".csv")
if ( file.exists((filename)) ) {
  cdfScore_q_chalky_given_uv_array = readRDS(filename)
} else {
  cdfScore_q_chalky_given_uv_array <- array(dim=c(m+1,m+1,m+1))
  for (u in 0:m) {
    for (v in 0:m) {
      for (k in 0:m) {
        print(paste0("precomputing cdfScore_q_chalky_given_uv_array", "u=",u,", v=",v,", k=",k))
        cdfScore_q_chalky_given_uv_array[u+1,v+1,k+1] = cdfScore_q_chalky_given_uv(m,k,u,v)
      }
    }
  }
  dimnames(cdfScore_q_chalky_given_uv_array) = list(paste0("u",0:m), paste0("v",0:m), paste0("k",0:m))
  saveRDS(cdfScore_q_chalky_given_uv_array, filename)
}

cdfScore_q_chalky_given_uv <- function(m,k,u,v) {
  if (k < 0) {
    0
  } else if (k > m) {
    1
  } else {
    cdfScore_q_chalky_given_uv_array[u+1,v+1,k+1]
  }
}
### check
# cdfScore_q_chalky_given_uv(m=m,k=30,u=20,v=20)
# cdfScore_q_chalky_given_uv(m=m,k=30,u=20,v=40)

cdfMaxScore_q_chalky <- function(m,p,q,k) {
  # print("a2")
  vec = matrix(0, m+1, m+1)
  for (u in 0:m) {
    for (v in 0:m) {
      vec[u+1,v+1] = cdfScore_q_chalky_given_uv(m,k,u,v)^n * dbinom(u, size=m, prob=1-p) * dbinom(v, size=m, prob=1-q)
    }
  }
  sum(vec)
}
### check
# cdfScore_q_chalky(m=m,p=0.7,q=0.5,k=30)
# cdfScore_q_chalky(m=m,p=0.7,q=0.5,k=40)
# cdfScore_q_chalky(m=m,p=0.7,q=0.5,k=50)
# cdfScore_q_chalky(m=m,p=0.7,q=0.75,k=30)
# cdfScore_q_chalky(m=m,p=0.7,q=0.75,k=40)
# cdfScore_q_chalky(m=m,p=0.7,q=0.75,k=50)

eMaxScore_q_chalky <- function(m,n,p,q) {
  # print("a3")
  # print(paste0("eMaxScore_q_chalky, ", " n=", n, ", p=", p, ", q=", q))
  vec = numeric(m+1)
  for (k in 0:m) {
    vec[k+1] = 1 - cdfMaxScore_q_chalky(m,p,q,k)
  }
  sum(vec)
}
### check
# eMaxScore_q_chalky(m=m,n=10,p=0.7,q=0.5)
# eMaxScore_q_chalky(m=m,n=10,p=0.7,q=0.7)
# eMaxScore_q_chalky(m=m,n=10,p=0.7,q=0.9)

######################################################################
### Win Probability of n q-Chalky Brackets vs. k r-Chalky Brackets ###
######################################################################

WP_nq_vs_kr <- function(m,p,n,q,k,r) {
  # result = array(0, dim=c(m+1,m+1))
  result = array(dim=c(m+1,m+1))
  for (u in 0:m) {
    # print(paste("u =", u) )
    # browser()
    pu = dbinom(u, size=m, prob=1-p)
    for (l in 0:m) {
      # print(paste("c(u,l) =", u,l) )
     
      S1 = numeric(m+1)
      S2 = numeric(m+1)
      S3 = numeric(m+1)
      for (v in 0:m) {
        cdf_l1_uv = cdfScore_q_chalky_given_uv(m=m,k=l-1,u=u,v=v)
        cdf_l_uv = cdfScore_q_chalky_given_uv(m=m,k=l,u=u,v=v)
        
        S1[v+1] = cdf_l1_uv * dbinom(v, size=m, prob=1-q)
        S2[v+1] = cdf_l_uv  * dbinom(v, size=m, prob=1-r)
        S3[v+1] = cdf_l1_uv * dbinom(v, size=m, prob=1-r)
      }
      S1n = sum(S1)^n
      S2k = sum(S2)^k
      S3k = sum(S3)^k
      
      result[u+1,l+1] = pu*S1n*(S2k-S3k)
    }
  }
  1 - sum(result)
}
# ### check.  takes ~2 secs to run
# ### should be close to 1:
# WP_nq_vs_kr(m=m,p=0.5,n=1000000,q=0.5,k=10,r=0.5)
# WP_nq_vs_kr(m=m,p=1,n=10,q=1,k=10,r=0)
# WP_nq_vs_kr(m=m,p=0.9,n=100,q=0.9,k=100,r=0.1)
# ### should be close to 0:
# WP_nq_vs_kr(m=m,p=0.5,n=10,q=0.5,k=1000000,r=0.5)
# WP_nq_vs_kr(m=m,p=1,n=10,q=0,k=10,r=1)
# WP_nq_vs_kr(m=m,p=0.9,n=100,q=0.1,k=100,r=0.9)
# WP_nq_vs_kr(m=m,p=0,n=10,q=0.5,k=1000,r=0.1)
# ### should be 0.5:
# WP_nq_vs_kr(m=m,p=0,n=1,q=0.1,k=1,r=0.1) 
# WP_nq_vs_kr(m=m,p=0,n=100,q=0.1,k=100,r=0.1) 
# WP_nq_vs_kr(m=m,p=0,n=1000,q=0.1,k=1000,r=0.1) 





# WP_nq_vs_kr <- function(m,p,n,q,k,r) {
#   # result = array(dim=c(m+1,m+1,m+1,m+1))
#   result = array(0, dim=c(m+1,m+1,m+1,m+1))
#   for (u in 0:m) {
#     pu = dbinom(u, size=m, prob=1-p)
#     
#     print(paste("u =", u) )
#     # browser()
#     for (v in 0:m) {
#       pv = dbinom(u, size=m, prob=1-q)
#       for (w in 0:m) {
#         pw = dbinom(u, size=m, prob=1-r)
#         for (l in 0:m) {
#           # print(paste("c(u,v,w,l) =", u,v,w,l) )
#           
#           #FIXME
#           t1 = cdfScore_q_chalky_given_uv(m=m,k=l-1,u=u,v=v)^(n*k) 
#           t2 = cdfScore_q_chalky_given_uv(m=m,k=m-l,u=u,v=w) - cdfScore_q_chalky_given_uv(m=m,k=m-(l-1),u=u,v=w)
#             
#           result[u+1,v+1,w+1,l+1] = t1*t2*pu*pv*pw
#         }
#       }
#     }
#   }
#   1 - sum(result)
# }


#################################################
### Plotting ###
#################################################

##### tibbles containing expected score as a function of (n,p) for random brackets ##### 

color_vec =  c(
  "chalky3"="firebrick2", 
  "chalky10"="firebrick", 
  "random"="dodgerblue2"
)
name_vec1 = c(
  # "chalky3"="one\nchalky\nbracket\n(u_max=3)\n", 
  # "chalky10"="one\nchalky\nbracket\n(u_max=10)\n",
  "chalky3"="one\nchalky\nbracket\n(U=3)\n", 
  "chalky10"="one\nchalky\nbracket\n(U=10)\n",
  "random"="one\nrandom\nbracket\n"
)

my_palette_1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

my_palette_n1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:4]#[3:9]
)

my_palette_n2 = c(
  # rev(brewer.pal(name="Purples",n=9)[4:7]),
  # brewer.pal(name="Reds",n=9)[3:7]#[3:9]
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:6]#[3:9]
)

my_palette_nk1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:9]
)


plot_grid = expand.grid(p=seq(0.5, 0.99, length=101), n=10^(0:8)) %>% as_tibble()





