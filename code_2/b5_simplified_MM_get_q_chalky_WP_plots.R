
source("b5_simplified_MM_main.R")

df_WP_nq_vs_kr = read_csv("df_WP_nq_vs_kr.csv")

#############
### PLOTS ###
#############

df_WP_nq_vs_kr %>%
  ggplot(aes(x=p, y=wp)) +
  facet_wrap(~n) +
  geom_line()

