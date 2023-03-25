
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

GRID_results = read_csv("df_entropy_range_grid_results.csv")

GRID_results %>% arrange(opp_prob_method, scoring_method)





GRID_results %>%
  ggplot() +
  facet_wrap(~scoring_method) +
  geom_point(aes(y=opp_prob_method, x=k, color=factor(n)), 
             size=3)






