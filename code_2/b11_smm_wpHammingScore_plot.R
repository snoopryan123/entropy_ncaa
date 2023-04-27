

source("b11_smm_main.R")

plot_df = read_csv(paste0(output_folder,"plot_grid_wpHammingScore.csv"))

plot_df = plot_df %>% filter(n <= 1000 & k <= 1000)

my_palette_nk1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:9]
)

#############
### PLOTS ###
#############

#### WP, p, n, q, k, r

for (p_ in unique(plot_df$p)) {
  for (k_ in unique(plot_df$k)) {
  plot_wp1_kp =
    plot_df %>%
    filter(p == p_ & k == k_) %>%
    mutate(n_ = paste0("n = ", n)) %>%
    ggplot(aes(y=q, x=r)) +
    facet_wrap(~n_) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(title=paste0("k = ", k_, ", p = ", p_)) +
    geom_tile(aes(fill=wp)) +
    geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
    geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
    geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
    # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
    scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
  plot_wp1_kp
  ggsave(
    paste0(output_folder, "plot_wpHammingScore_k",format(k_, scientific=T),"p",p_,".png"), 
    plot_wp1_kp,
    # width=12,height=10
    width=10,height=8
  )
  
  # plot_wp2_kp =
  #   plot_df %>%
  #   filter(p == p_ & k == k_) %>%
  #   mutate(n_ = paste0("n = ", n)) %>%
  #   ggplot(aes(x=q, color=factor(r), y=wp)) +
  #   facet_wrap(~n_) +
  #   theme(panel.spacing = unit(2, "lines")) +
  #   # geom_vline
  #   labs(title=paste0("k = ", k_, ", p = ", p_)) +
  #   geom_line(linewidth=1) +
  #   geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  #   ylab("win probability") +
  #   scale_color_manual(name="r", values=my_palette_nk1)
  # plot_wp2_kp
  # ggsave(paste0(output_folder, "plot_wp1HammingScore_k",k_,"p",p_,".png"), plot_wp2_kp,
  #        width=12,height=8)
  }
}



# for (p_ in unique(plot_df$p)) {
#   
#   # qn_star =
#   #   plot_df %>%
#   #   filter(p == p_) %>%
#   #   group_by(p,k,r) %>%
#   #   filter(
#   #     wp == max(wp)
#   #   ) %>%
#   #   filter(
#   #     n == max(n)
#   #   ) %>%
#   #   ungroup()
#   # qn_star
#   
#   q_star = 
#     plot_df %>%
#     filter(p == p_) %>%
#     group_by(p,k,r,n) %>%
#     filter(
#       wp == max(wp)
#     ) %>%
#     ungroup()
#   q_star
#   
#   plot_q_star_nkrp = 
#     q_star %>%
#     filter(r < 1) %>%
#     mutate(n_ = paste0("n = ", n)) %>%
#     ggplot(aes(y=q, x=r, color=factor(k))) +
#     facet_wrap(~n_) +
#     theme(panel.spacing = unit(2, "lines")) +
#     ylab(TeX("$q^*$")) +
#     geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
#     geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
#     geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
#     # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
#     labs(title=paste0("p = ", p_)) +
#     scale_color_manual(name="k", values=my_palette_nk1) +
#     geom_point(size=4)
#   # geom_line(linewidth=1)
#   plot_q_star_nkrp
#   ggsave(paste0(output_folder,"plot_wpHammingScore_qStar_nkr","p",p_,".png"), plot_q_star_nkrp,
#          width=14,height=8)
# }






