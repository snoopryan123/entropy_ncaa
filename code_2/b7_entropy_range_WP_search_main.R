
source("a2_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

### grid to search over
GRID = expand.grid(
  n = 10^(2:4),
  k = 10^(2:4),
  opp_prob_method = c("naive_chalky", "P_538_2022", "naive_random", "super_chalky")
) %>%
  filter(n <= k) %>%
  # filter(n < k) %>% 
  arrange(n,k)
GRID


