
source("../a0_loadStuff.R")

H <- function(p) {
  -(p*log(p,base=2) + (1-p)*log(1-p,base=2))
}

# ### check
# ps = seq(0,1,by=0.025)
# plot(ps, H(ps))

ratio_of_typical_set <- function(p,m) {
  2^(-(1-H(p))*m)
}

H(p=0.75)
ratio_of_typical_set(p=0.75, m=63)
1/ratio_of_typical_set(p=0.75, m=63)




