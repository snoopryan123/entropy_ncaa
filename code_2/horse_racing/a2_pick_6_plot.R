
source("a2_pick_6_main.R")

plot_df0 = read_csv("plots/plot_grid_eProfit_v1.csv")
plot_df1 = plot_df0 %>% 
  ### get just one opp. strategy
  filter(lambda_opp == 0.8) %>%
  ### clean the df
  select(-c(E_W_ratio, t)) %>% 
  relocate(C, .before=n) %>%
  relocate(alpha, .after=C) %>%
  relocate(n, .before=C) %>%
  relocate(lambda, .after=n) %>%
  relocate(index_a, .after= lambda) %>%
  relocate(k, .after=alpha) %>%
  relocate(lambda_opp, .after=k) %>%
  relocate(E_profit, .after=lambda_opp)
plot_df = plot_df1 %>%
  ### get max. profit
  group_by(C,alpha,k,lambda_opp) %>%
  mutate(
    max_E_profit = max(E_profit),
    argmax_n = n[(which(max_E_profit == E_profit))[1]],
    argmax_lambda = lambda[(which(max_E_profit == E_profit))[1]],
    argmax_index_a = index_a[(which(max_E_profit == E_profit))[1]]
  ) %>%
  ungroup() %>%
  ### string descriptors
  # mutate(alpha_str = TeX(paste0("$\\alpha$ = ", alpha))) #%>%
  mutate(alpha_str = paste0("alpha = ", alpha)) %>%
  mutate(C_str = paste0("C = ", C)) %>%
  mutate(C_str = fct_reorder(C_str, C)) 
plot_df

### profit diagnostics
sum(plot_df$E_profit > 0)
nrow(plot_df)
sum(plot_df$E_profit > 0)/nrow(plot_df)
profit_df = plot_df  %>% filter(E_profit > 0) 

#########################################################
### Visualize Expected Profit for Pick 6 Horse Racing ###
#########################################################

# C_ = 38016
# alpha_ = 0.05
# n_ = 10
# k_ = 100000

# plot_df %>% 
#   filter(C == C_) %>%
#   filter(alpha == alpha_) %>%
#   filter(n == n_) %>%
#   filter(k == k_) %>%
#   filter(n == n_) %>%
#   # filter(a == 3) %>%
#   # filter(a == 6) %>%
#   ggplot() +
#   # ggplot(aes(x = lambda_opp, y = lambda, fill=E_profit)) +
#   geom_tile(aes(x = lambda, y = a, fill=E_profit))
# 
# 
# plot_df %>% 
#   filter(C == C_) %>%
#   filter(alpha == alpha_) %>%
#   # filter(n == n_) %>%
#   filter(k == k_) %>%
#   # filter(n == n_) %>%
#   filter(lambda == 0.8) %>%
#   filter(a == 3) %>%
#   filter(n <= 1e3) %>%
#   # filter(a == 6) %>%
#   ggplot() +
#   # ggplot(aes(x = lambda_opp, y = lambda, fill=E_profit)) +
#   geom_line(aes(x = lambda_opp, y = E_profit, 
#                 color=factor(n), linetype=factor(a)))
# 


appender <- function(string) {
  TeX(paste("$\\alpha = $", string)) 
}

plot_df %>%
  # filter(k < 1e6) %>%
  filter(k > 1e4) %>%
  select(
    C_str,alpha,#alpha_str,
    lambda_opp,k,
    # n,
    max_E_profit,
    # argmax_n,argmax_lambda,argmax_a
  ) %>% 
  distinct() %>%
  ggplot(aes(x = factor(k))) +
  facet_wrap(~alpha+C_str, nrow=3,
             # labeller = as_labeller(appender, default = label_parsed)
             ) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  # theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  theme(axis.text.x = element_text(size=10, angle=90)) +
  # geom_text(aes(label = argmax_n), x=0.9, y=19) +
  # geom_text(aes(label = argmax_lambda), x=0.9, y=14) +
  # geom_text(aes(label = argmax_a), x=0.9, y=10) +
  geom_point(aes(y = max_E_profit), size=3) 



