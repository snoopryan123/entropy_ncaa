
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
  mutate(
    argmax_n_str = paste0("n=", argmax_n),
    argmax_lambda_str = paste0("lambda=", argmax_lambda),
    argmax_index_a_str = paste0("idx(a)=", argmax_index_a)
  ) %>%
  # rowwise() %>%
  # mutate(argmax_lambda_str = TeX(paste0("$\\lambda$=", argmax_lambda))) %>%
  # ungroup() %>%
  mutate(n_str = paste0("n = ", n)) %>%
  mutate(n_str = fct_reorder(n_str, n)) %>%
  mutate(k_str = paste0("k = ", k)) %>%
  mutate(k_str = fct_reorder(k_str, k)) %>%
  mutate(alpha_str = paste0("alpha = ", alpha)) %>%
  mutate(C_str = paste0("C = ", C)) %>%
  mutate(C_str = fct_reorder(C_str, C)) 
plot_df

### profit diagnostics
sum(plot_df$E_profit > 0)
nrow(plot_df)
sum(plot_df$E_profit > 0)/nrow(plot_df)
profit_df = plot_df  %>% filter(E_profit > 0) 

### 
m = colSums(P != 0) # number of horses in each race
prod(m)


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

###########################
### Max Expected Profit ###
###########################

k_range_1 = c(5e5, Inf)
k_range_2 = c(1e4,1e5)
k_range_3 = c(1,1e4)
k_ranges = list(k_range_1, k_range_2, k_range_3)

for (i in 1:length(k_ranges)) {
  k_range = k_ranges[[i]]
  plot_eMaxProfit = plot_df %>%
    filter(min(k_range) <= k & k <= max(k_range)) %>%
    # filter(min(k_range_2) <= k & k <= max(k_range_2)) %>%
    select(
      C_str,alpha_str,#alpha_str,
      lambda_opp,k,
      max_E_profit,
      argmax_n_str,
      argmax_index_a_str,
      argmax_lambda_str
    ) %>% 
    distinct() %>%
    ggplot(aes(x = factor(k))) +
    facet_wrap(~alpha_str+C_str, nrow=3) +
    xlab("k") +
    # ylab(TeX(paste0("expected profit using the best (n,$\\lambda$,index(a)) strategy"))) +
    # theme(axis.title.y = element_text(size=18)) +
    ylab(TeX(paste0("expected profit using our best strategy"))) +
    theme(axis.text.y = element_text(size=12)) +
    theme(axis.text.x = element_text(size=12, angle=90)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    # geom_text(aes(label = argmax_n_str, y=max_E_profit+15), size=3) +
    # geom_text(aes(label = argmax_index_a_str, y=max_E_profit+25), size=3) +
    # geom_text(aes(label = argmax_lambda_str, y=max_E_profit+35), size=3) +
    geom_point(aes(y = max_E_profit), size=4) 
  # plot_eMaxProfit
  ggsave(paste0(output_folder, "plot_eMaxProfit_krange",i,".png"),
         width=8, height=7
         # width=11, height=9
         )
}

#############################
### Expected Profit vs. n ###
#############################

n_range_1 = c(5e5, Inf)
n_range_2 = c(1e4,1e5)
n_range_3 = c(1,1e4)
n_ranges = list(n_range_1, n_range_2, n_range_3)

for (i in 1:length(n_ranges)) {
  n_range = n_ranges[[i]]
  plot_eProfit_vs_n = plot_df %>%
    filter(min(n_range) <= n & n <= max(n_range)) %>%
    filter(lambda == 1) %>%
    filter(alpha == 0.05, C == 38016) %>%
    select(
      C_str,alpha_str,
      lambda_opp,k,k_str,
      E_profit,
      index_a,n
    ) %>% 
    distinct() %>%
    ggplot(aes(x = factor(n), y=E_profit, color=factor(index_a))) +
    xlab("n") +
    labs(title=TeX("$\\lambda = 1, \\alpha = 0.05, C = 38016$"), color="index(a)") +
    theme(axis.text.x = element_text(size=12, angle=90)) +
    facet_wrap(~k_str) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    geom_point(size=4, alpha=0.7) +
    ylab("expected profit") 
  # plot_eProfit_vs_n
  ggsave(paste0(output_folder, "plot_eProfit_vs_n",i,".png"),
         width=11, height=9
  )
}

#############################
### Expected Profit vs. n ###
#############################

n_range_1 = c(5e5, Inf)
n_range_2 = c(1e4,1e5)
n_range_3 = c(1,1e4)
n_ranges = list(n_range_1, n_range_2, n_range_3)

for (C_ in unique(plot_df$C)) {
  for (i in 1:length(n_ranges)) {
    n_range = n_ranges[[i]]
    plot_eProfit_vs_indexa = plot_df %>%
      filter(min(n_range) <= n & n <= max(n_range)) %>%
      filter(lambda == 1) %>%
      # filter(alpha == 0.05, C == 38016) %>%
      filter(alpha == 0.05, C == C_) %>%
      # filter(k <= 50000) %>%
      select(
        C_str,alpha_str,#alpha_str,
        lambda_opp,k,k_str,
        E_profit,
        index_a,n
      ) %>% 
      distinct() %>%
      ggplot(aes(x = factor(index_a), y=E_profit, color=factor(n))) +
      xlab("index(a)") +
      labs(title=TeX(paste0("$\\lambda = 1, \\alpha = 0.05,$ C = ", C_)), color="n") +
      # ggplot(aes(x = factor(n), y=E_profit, color=factor(index_a))) +
      # xlab("n") +
      # labs(title=TeX("$\\lambda = 1, \\alpha = 0.05, C = 38016$"), color="index(a)") +
      theme(axis.text.x = element_text(size=12)) +
      facet_wrap(~k_str) +
      geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
      geom_point(size=4, alpha=0.7) +
      ylab("expected profit") 
    # plot_eProfit_vs_indexa
    ggsave(paste0(output_folder, "plot_eProfit_vs_indexa_",i,"_C",C_, ".png"),
           # width=8, height=7
           width=15, height=9
    )
  }
}
