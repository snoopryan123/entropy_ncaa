
source("a2_pick_6_main.R")

# version_ = 1
version_ = 2
plot_df0 = read_csv(paste0("plots/plot_grid_eProfit_v",version_,".csv"))
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

alpha_ = 0.05
k_ = ks[1]

# C_ = 38016
# C_ = 100000
# alpha_ = 0.05
# k_ = 1000000

# data.frame(
#   plot_df1 %>% 
#     group_by(C,alpha,k,lambda_opp) %>%
#     summarise(
#       max_E_profit = max(E_profit),
#       n = n[which(E_profit == max(E_profit))[1]]
#     )
# )

my_palette = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

if (version_ == 1) {
  plot_picksix_1 = 
    plot_df1 %>%
    # filter(lambda_opp < 4) %>%
    # filter(C == C_, alpha == alpha_, k == k_) %>%
    filter(C %in% c(38016, 1e6)) %>%
    filter(alpha == alpha_, k == k_) %>%
    filter(E_profit == max_E_profit) %>%
    mutate(C_str = paste0("C = ", C),
           C_str = fct_reorder(C_str, C),
           lambda_opp = round(lambda_opp, 3),
           lambda_a = paste0("\u03BB = ", lambda, ", phi = ", phi)
    ) %>%
    # ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n))) +
    # labs(title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_)), color="n") +
    # ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n), shape=factor(lambda_a))) +
    ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n))) +
    labs(
      title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_)), 
      color="n",
      shape="strategy"
    ) +
    theme(axis.text.x = element_text(size=12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~ C_str, scales="free_y") +
    xlab(TeX("$\\lambda_{opp}$")) +
    theme(legend.text = element_text(size=12)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    geom_point(size=3, stroke=2) +
    ylab("expected profit") 
  # plot_picksix_1
  ggsave(paste0(output_folder, "plot_picksix_1_v",version_,".png"), plot_picksix_1, width=11, height=5)
} else if (version_ == 2) {
  C_ = 5e5
  plot_picksix_1A = 
    plot_df1 %>%
    filter(C == C_) %>%
    filter(alpha == alpha_, k == k_) %>%
    # filter(E_profit == max_E_profit) %>%
    mutate(C_str = paste0("C = ", C),
           C_str = fct_reorder(C_str, C),
           lambda_opp = round(lambda_opp, 3),
           lambda_a = paste0("\u03BB = ", lambda, ", phi = ", phi)
    ) %>%
    filter(n %in% c(1000, 5000, 10000, 25000)) %>%
    group_by(alpha,k,C,n,lambda_opp) %>%
    mutate(
      max_E_profit = max(E_profit),
      lambda_a = lambda_a[max(E_profit) == E_profit],
      lambda_a = fct_reorder(lambda_a, lambda)
    ) %>%
    # distinct() %>%
    ungroup() %>%
    ggplot(aes(
        x = factor(lambda_opp),
        # x = lambda_opp, 
        y = max_E_profit,
       color = factor(n),
       # shape = factor(n),
       # y=E_profit,
       # shape=factor(lambda_a)
       shape=lambda_a
    )) +
    labs(
      title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_, ", C = ", C_)), 
      color="n",
      # shape = "n"
      shape="strategy"
    ) +
    theme(axis.text.x = element_text(size=12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(TeX("$\\lambda_{opp}$")) +
    theme(legend.text = element_text(size=12)) +
    scale_color_manual(values=my_palette) +
    # scale_color_manual(values=c("firebrick", "dodgerblue2", "forestgreen")) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    geom_point(size=4, stroke=2) +
    # geom_point(size=1, stroke=2) +
    # geom_line(linewidth=1) +
    ylab("expected profit") 
  plot_picksix_1A
  ggsave(paste0(output_folder, "plot_picksix_1A_v",version_,".png"), 
         plot_picksix_1A, width=10, height=6)
  
  plot_picksix_1B = 
    plot_df1 %>%
    filter(C == C_) %>%
    filter(alpha == alpha_, k == k_) %>%
    # filter(E_profit == max_E_profit) %>%
    mutate(C_str = paste0("C = ", C),
           C_str = fct_reorder(C_str, C),
           lambda_opp = round(lambda_opp, 3),
           lambda_a = paste0("\u03BB = ", lambda, ", phi = ", phi)
    ) %>%
    filter(n %in% c(1000, 5000, 10000, 25000)) %>%
    group_by(alpha,k,C,n,lambda_opp) %>%
    mutate(
      max_E_profit = max(E_profit),
      lambda_a = lambda_a[max(E_profit) == E_profit]
    ) %>%
    # distinct() %>%
    ungroup() %>%
    ggplot(aes(
      x = factor(lambda_opp),
      # x = lambda_opp, 
      y = max_E_profit,
      color = factor(n),
      shape = factor(n),
      # y=E_profit,
      # shape=factor(lambda_a)
    )) +
    labs(
      title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_, ", C = ", C_)), 
      color="n",
      shape = "n"
      # shape="strategy"
    ) +
    theme(axis.text.x = element_text(size=12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(TeX("$\\lambda_{opp}$")) +
    theme(legend.text = element_text(size=12)) +
    # scale_color_manual(values=c("firebrick", "dodgerblue2", "forestgreen")) +
    scale_color_manual(values=my_palette) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    geom_point(size=4, stroke=2) +
    # geom_point(size=1, stroke=2) +
    # geom_line(linewidth=1) +
    ylab("expected profit") 
  plot_picksix_1B
  ggsave(paste0(output_folder, "plot_picksix_1B_v",version_,".png"), 
         plot_picksix_1B, width=10, height=6)
  
  # C_ = 5e5
  # plot_picksix_1 = 
  #   plot_df1 %>%
  #   filter(C == C_) %>%
  #   filter(alpha == alpha_, k == k_) %>%
  #   filter(E_profit == max_E_profit) %>%
  #   mutate(C_str = paste0("C = ", C),
  #          C_str = fct_reorder(C_str, C),
  #          lambda_opp = round(lambda_opp, 3),
  #          lambda_a = paste0("\u03BB = ", lambda, ", phi = ", phi)
  #   ) %>%
  #   ggplot(aes(x = factor(lambda_opp), 
  #              y=max_E_profit,
  #              color=factor(n),
  #              shape=factor(lambda_a)
  #   )) +
  #   labs(
  #     title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_, ", C = ", C_)), 
  #     color="n",
  #     shape="strategy"
  #   ) +
  #   theme(axis.text.x = element_text(size=12)) +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #   xlab(TeX("$\\lambda_{opp}$")) +
  #   theme(legend.text = element_text(size=12)) +
  #   # scale_color_manual(values=c("firebrick", "dodgerblue2", "forestgreen")) +
  #   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  #   geom_point(size=4, stroke=2) +
  #   ylab("expected profit") 
  # plot_picksix_1
  # ggsave(paste0(output_folder, "plot_picksix_1_v",version_,".png"), plot_picksix_1, width=7, height=5)
  
  # plot_picksix_1 = 
  #   plot_df1 %>%
  #   # filter(lambda_opp < 4) %>%
  #   # filter(C == C_, alpha == alpha_, k == k_) %>%
  #   # filter(C %in% c(38016, 1e6)) %>%
  #   # filter(C %in% c(1e6)) %>%
  #   filter(C >= 5e5) %>%
  #   filter(alpha == alpha_, k == k_) %>%
  #   filter(E_profit == max_E_profit) %>%
  #   mutate(C_str = paste0("C = ", C),
  #          C_str = fct_reorder(C_str, C),
  #          lambda_opp = round(lambda_opp, 3),
  #          lambda_a = paste0("\u03BB = ", lambda, ", phi = ", phi)
  #   ) %>%
  #   # ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n))) +
  #   # ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(lambda_a))) +
  #   ggplot(aes(x = factor(lambda_opp), y=max_E_profit, color=factor(n), shape=factor(lambda_a))) +
  #   labs(
  #     title=TeX(paste0("$\\alpha$ = 0.05, k = ", k_)), 
  #     color="n",
  #     shape="strategy"
  #   ) +
  #   theme(axis.text.x = element_text(size=12)) +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #   facet_wrap(~ C_str, scales="free_y") +
  #   xlab(TeX("$\\lambda_{opp}$")) +
  #   theme(legend.text = element_text(size=12)) +
  #   scale_color_manual(values=c("firebrick", "dodgerblue2", "forestgreen")) +
  #   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  #   geom_point(size=3, stroke=2) +
  #   ylab("expected profit") 
  # # plot_picksix_1
  # ggsave(paste0(output_folder, "plot_picksix_1_v",version_,".png"), plot_picksix_1, width=11, height=5)
}


