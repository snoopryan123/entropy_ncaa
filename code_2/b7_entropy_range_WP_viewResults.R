
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


w=0.3
GRID_results %>%
  ggplot(y=0:m, aes(color=factor(n), )) +
  facet_wrap(~scoring_method + opp_prob_method) +
  geom_segment(aes(y=hL_star, yend=hU_star, x=log(k,base=10)+w, xend=log(k,base=10)+w), position = "jitter", size=1)
  
  # geom_point(aes(y=opp_prob_method, x=k, color=factor(n)), 
             # size=3)

my_palette_a = c(
  rev(brewer.pal(name="PuRd",n=9)[3:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

GRID_results %>%
  ggplot(y=0:m, aes(color=factor(n), )) +
  facet_wrap(~scoring_method + opp_prob_method) +
  geom_point(aes(y=hU_star, x=log(k,base=10)), size=5) +
  scale_color_manual(values=my_palette_a)





