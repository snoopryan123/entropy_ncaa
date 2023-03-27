
source("a2_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

# ### grid to search over
# GRID = expand.grid(
#     n = 10^(0:6),
#     # k = c(10,100,1.73*1e7),
#     k = c(10,100,10^6),
#     opp_prob_method = c("naive_chalky", "P_538_2022"),
#     scoring_method = c("ESPN", "num_correct")
#   ) %>%
#   filter(n <= k) %>% 
#   arrange(k)
# GRID

# ### grid to search over
# GRID = expand.grid(
#   # n = 10^(0:6),
#   # k = c(10,100,1.73*1e7),
#   # k = c(10,100,10^6),
#   # opp_prob_method = c("naive_chalky", "P_538_2022"),
#   #### scoring_method = c("ESPN", "num_correct")
#   n = 10^(0:3),
#   k = 10^(0:3),
#   opp_prob_method = c("naive_chalky")
# ) %>%
#   filter(n <= k) %>% 
#   arrange(n,k)
# GRID


### grid to search over
GRID = expand.grid(
  # n = 10^(0:6),
  # k = c(10,100,1.73*1e7),
  # k = c(10,100,10^6),
  # opp_prob_method = c("naive_chalky", "P_538_2022"),
  #### scoring_method = c("ESPN", "num_correct")
  n = 10^(2:4),
  k = 10^(2:4),
  # opp_prob_method = c("naive_chalky")
  opp_prob_method = c("naive_chalky", "P_538_2022", "naive_random")
) %>%
  filter(n <= k) %>% 
  # arrange(opp_prob_method,n,k)
  arrange(k,n)
  # arrange(n,k)
GRID


