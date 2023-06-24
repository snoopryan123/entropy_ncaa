
source("a2_pick_6_main.R")

plot_df0 = read_csv("plots/plot_grid_eProfit_v1.csv")
plot_df = plot_df0 %>% 
  select(-c(E_W_ratio, t)) %>% 
  relocate(C, .before=n) %>%
  relocate(alpha, .after=C) %>%
  relocate(n, .before=C) %>%
  relocate(lambda, .after=n) %>%
  relocate(a, .after= lambda) %>%
  relocate(k, .after=alpha) %>%
  relocate(E_profit, .after=a_opp) %>%
  ### get max. profit
  group_by(C,alpha,lambda_opp,a_opp,k) %>%
  mutate(
    max_E_profit = max(E_profit),
    argmax_n = n[(which(max_E_profit == E_profit))[1]],
    argmax_lambda = lambda[(which(max_E_profit == E_profit))[1]],
    argmax_a = a[(which(max_E_profit == E_profit))[1]]
  ) %>%
  ungroup() %>%
  ### string descriptors
  mutate(C_str = paste0("C = ", C)) %>%
  mutate(C_str = fct_reorder(C_str, C)) %>%
  # mutate(alpha_str = TeX(paste0("$\\alpha$ = ", alpha))) #%>%
  mutate(alpha_str = paste0("alpha = ", alpha)) #%>%
plot_df

sum(plot_df$E_profit > 0)

profit_df = plot_df %>% filter(n < k) %>% filter(E_profit > 0) %>% filter(C < 1e5)

#########################################################
### Visualize Expected Profit for Pick 6 Horse Racing ###
#########################################################

C_ = 38016
alpha_ = 0.05
n_ = 10
k_ = 100000

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


plot_df %>%
  # filter(C == C_) %>%
  # filter(alpha == alpha_) %>%
  filter(C < 1e6) %>%
  filter(k < 1e6) %>%
  filter(k < 1e5) %>%
  filter(k > 5000) %>%
  distinct(
    C_str,alpha_str,lambda_opp,a_opp,k,
    max_E_profit,
    # argmax_n,argmax_lambda,argmax_a
    ) %>%
  # mutate(argmax_n = paste0("n = ", argmax_n)) %>%
  # mutate(argmax_lambda = paste0("lambda = ", argmax_lambda)) %>%
  # mutate(argmax_a = paste0("a = ", argmax_a)) %>%
  ggplot() +
  facet_wrap(~alpha_str+C_str, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  # geom_text(aes(label = argmax_n), x=0.9, y=19) +
  # geom_text(aes(label = argmax_lambda), x=0.9, y=14) +
  # geom_text(aes(label = argmax_a), x=0.9, y=10) +
  geom_line(aes(x = lambda_opp, color=factor(k), y = max_E_profit)) 


