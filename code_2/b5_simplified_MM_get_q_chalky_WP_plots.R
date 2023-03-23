
source("b5_simplified_MM_main.R")

df_WP_nq_vs_kr = read_csv("df_WP_nq_vs_kr.csv")

#############
### PLOTS ###
#############

#### WP, p, n, q, k, r

p_ = 0.75
ks = 10^(0:8)
for (k_ in ks) {
  plot_wp1_kp =
    df_WP_nq_vs_kr %>%
    filter(k == k_) %>%
    mutate(n_ = paste0("n = ", n)) %>%
    ggplot(aes(x=q, y=r)) +
    facet_wrap(~n_) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(title=paste0("k = ", k_, ", p = ", p_)) +
    geom_tile(aes(fill=wp)) +
    geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
    geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
    geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
    # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
    scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
  ggsave(paste0("plot_thm1/plot_wp1_k",k_,"p",p_,".png"), plot_wp1_kp,
         width=12,height=8)
  
  plot_wp2_kp = 
    df_WP_nq_vs_kr %>%
    mutate(n_ = paste0("n = ", n)) %>%
    filter(k == k_) %>%
    ggplot(aes(x=q, color=factor(r), y=wp)) +
    facet_wrap(~n_) +
    theme(panel.spacing = unit(2, "lines")) +
    # geom_vline
    labs(title=paste0("k = ", k_, ", p = ", p_)) +
    geom_line(linewidth=1) +
    geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
    ylab("win probability") +
    scale_color_manual(name="r", values=my_palette_nk1)
  ggsave(paste0("plot_thm1/plot_wp2_k",k_,"p",p_,".png"), plot_wp2_kp,
         width=12,height=8)
}

########################################
### CHOOSE OPTIMAL q*,n* GIVEN P,K,R ###
########################################

qn_star = df_WP_nq_vs_kr %>%
  select(p,k,r,n,q,wp) %>%
  group_by(p,k,r) %>%
  filter(
    wp == max(wp)
  ) %>%
  filter(
    n == max(n)
  ) %>%
  ungroup()
qn_star

q_star = df_WP_nq_vs_kr %>%
  select(p,k,r,n,q,wp) %>%
  group_by(p,k,r,n) %>%
  filter(
    wp == max(wp)
  ) %>%
  ungroup()
q_star


plot_q_star_nkrp = q_star %>%
  filter(r < 1) %>%
  mutate(n_ = paste0("n = ", n)) %>%
  ggplot(aes(y=q, x=r, color=factor(k))) +
  facet_wrap(~n_) +
  theme(panel.spacing = unit(2, "lines")) +
  ylab(TeX("$q^*$")) +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
  # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
  labs(title=paste0("p = ", p_)) +
  scale_color_manual(name="k", values=my_palette_nk1) +
  geom_point(size=2)
  # geom_line(linewidth=1)
# plot_q_star_nkrp
ggsave(paste0("plot_thm1/plot_q_star_nkr","p",p_,".png"), plot_q_star_nkrp,
       width=12,height=8)
