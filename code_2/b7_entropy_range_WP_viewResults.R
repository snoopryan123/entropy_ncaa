
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

GRID_results = read_csv("df_entropy_range_grid_results.csv")

my_palette_a = c(
  rev(brewer.pal(name="PuRd",n=9)[3:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

GRID_results %>%
  pivot_longer(c("hU_star_espn", "hU_star_num_correct")) %>%
  rename(hU_star = value) %>%
  rename(scoring_method = name) %>%
  mutate(scoring_method = str_sub(scoring_method, start=9)) %>%
  ggplot(y=0:m, aes(color=factor(n), )) +
  facet_wrap(~ scoring_method + opp_prob_method) +
  # scale_color_manual(values=my_palette_a) +
  geom_point(aes(y=hU_star, x=log(k,base=10)), size=5) 




