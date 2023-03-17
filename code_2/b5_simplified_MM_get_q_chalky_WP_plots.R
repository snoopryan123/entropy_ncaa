
source("b5_simplified_MM_main.R")

df_WP_nq_vs_kr = read_csv("df_WP_nq_vs_kr.csv")

#############
### PLOTS ###
#############

#### WP, p, n, q, k, r

df_WP_nq_vs_kr %>%
  filter(k == 1e8) %>%
  filter(r == p) %>%
  filter(q == p) %>%
  ggplot(aes(x=p, y=wp)) +
  facet_wrap(~n) +
  geom_line()

