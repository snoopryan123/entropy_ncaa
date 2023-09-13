
source("a2_main.R")
output_folder = "./plot_13/"

plot_df = read_csv(paste0(output_folder, "plot_grid_wp_fmm_v1_fold_1.csv"))
plot_df_1 = plot_df %>% 
  pivot_longer(cols=c("wpESPN_scores", "wpHamming_scores")) %>%
  rename(scoring_function = name) %>%
  rename(wp = value) %>%
  mutate(scoring_function = ifelse(scoring_function == "wpESPN_scores", "ESPN", "Hamming")) %>%
  group_by(n,k,scoring_function) %>%
  mutate(max_wp = ifelse(wp==max(wp), wp, NA) ) %>%
  mutate(
    opp_prob_method = case_when(
      opp_prob_method == "naive_chalky" ~ "colloquially-chalky",
      TRUE ~ opp_prob_method
    )
  ) %>%
  ungroup()
plot_df_1

my_palette_h_wp = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

#######################################
### Plot Expected Max Hamming Score ###
#######################################

for (opp_ in unique(plot_df_1$opp_prob_method)) {
  for (k_ in unique(plot_df_1$k)) {
    plot_fmm_entropyTail_wp = 
      plot_df_1 %>%
      filter(k == k_) %>%
      filter(opp_prob_method == opp_) %>%
      mutate(
        # n_ = paste0("n = ", n),
        n_ = paste0(n),
        k_ = paste0(k),
        scoring_function = factor(scoring_function, levels=c("Hamming","ESPN"))
      ) %>%
      # filter(n >= 100) %>% #FIXME
      ggplot(aes(color = n_, x = h, y = wp)) + 
      facet_wrap(~scoring_function, scales="free_y") +
      geom_vline(xintercept = 48.7, color="gray60", linetype="dashed") +
      geom_line(linewidth=1) +
      ylab("win probability") +
      labs(title=paste0("k = ", k_, ", ", opp_, " opponents")) +
      scale_color_manual(name="n", values=my_palette_h_wp) +
      geom_point(aes(y=max_wp), shape=21, size=2.5, stroke=0.5)
    # plot_fmm_entropyTail_wp
    ggsave(paste0(output_folder, "plot_fmm_entropyTail_wp_k=",k_,"_opp=",opp_,".png"),
           width=12, height=5)
  }
}

