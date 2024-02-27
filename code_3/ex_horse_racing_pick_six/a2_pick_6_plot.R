
source("a2_pick_6_main.R")

plot_df0 = read_csv("plots/plot_grid_eProfit_v1.csv")
plot_df0
plot_df1 = 
  plot_df0 %>% 
  group_by(C,alpha,k,lambda_opp) %>%
  mutate(max_E_profit = max(E_profit)) %>%
  ungroup()
plot_df1

Cs = unique(plot_df1$C)
alphas = unique(plot_df1$alpha)
ks = unique(plot_df1$k)

Cs
alphas
ks

# C_ = 38016
# C_ = 100000
alpha_ = 0.05
k_ = 1000000

# data.frame(
#   plot_df1 %>% 
#     group_by(C,alpha,k,lambda_opp) %>%
#     summarise(
#       max_E_profit = max(E_profit),
#       n = n[which(E_profit == max(E_profit))[1]]
#     )
# )

plot_picksix_1 = 
  plot_df1 %>%
  # filter(lambda_opp < 4) %>%
  # filter(C == C_, alpha == alpha_, k == k_) %>%
  filter(C %in% c(38016, 1e6)) %>%
  filter(alpha == alpha_, k == k_) %>%
  filter(E_profit == max_E_profit) %>%
  mutate(C_str = paste0("C = ", C),
         C_str = fct_reorder(C_str, C)) %>%
  mutate(lambda_opp = round(lambda_opp, 3)) %>%
  ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n))) +
  labs(title=TeX("$\\lambda = 1, \\alpha = 0.05, k = 1000000$"), color="n") +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~ C_str, scales="free_y") +
  xlab(TeX("$\\lambda_{opp}$")) +
  theme(legend.text = element_text(size=12)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  geom_point(size=2, shape=21, stroke=2) +
  ylab("expected profit") 
# plot_picksix_1
ggsave(paste0(output_folder, "plot_picksix_1.png"), plot_picksix_1, width=11, height=5)

