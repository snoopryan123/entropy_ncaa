
source("b5_simplified_MM_main.R")

df_WP_nq_vs_kr = read_csv("df_WP_nq_vs_kr.csv")

# get_WP <- function(p_,n_,q_,k_,r_) {
#   # browser()
#   (df_WP_nq_vs_kr %>% filter(p==p_&n==n_&q==q_&k==k_&r==r_))$wp
# }
# get_WP(p=.6,n=100,q=.6,k=1e8,r=.6)
# get_WP(p=.95,n=1e4,q=.95,k=1e8,r=.95)
# get_WP(p=.95,n=1e7,q=.95,k=1e8,r=.95)

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

df_WP_nq_vs_kr %>%
  filter(p == .75) %>%
  filter(q == .75) %>%
  filter(r == .75) %>%
  ggplot(aes(x=log(k,base=10), y=wp)) +
  facet_wrap(~n) +
  geom_line() +
  geom_point()

df_WP_nq_vs_kr %>%
  filter(p == .75) %>%
  filter(q == .75) %>%
  filter(r == .75) %>%
  ggplot(aes(x=log(n,base=10), y=log(k,base=10))) +
  # geom_line() +
  geom_point()

plot_grid_pnqkr %>%
  filter(p == .75) %>%
  filter(q == .75) %>%
  filter(r == .75) %>%
  ggplot(aes(x=log(n,base=10), y=log(k,base=10))) +
  # geom_line() +
  geom_point()

plot_grid_pnqkr %>%
  filter(p == .95) %>%
  filter(q == .95) %>%
  filter(r == .95) %>%
  # facet_wrap(~factor(p)) +
  ggplot(aes(x=log(n,base=10), y=log(k,base=10))) +
  # geom_line() +
  geom_point()
