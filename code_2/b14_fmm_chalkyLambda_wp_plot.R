
source("a2_main.R")
output_folder = "./plot_14/"

plot_df = bind_rows(
  read_csv(paste0(output_folder, "plot_grid_wp_fmm_chalkyLambda_v1_fold_1.csv")),
  read_csv(paste0(output_folder, "plot_grid_wp_fmm_chalkyLambda_v1_fold_2.csv"))
)

plot_df_1 = plot_df %>% 
  pivot_longer(cols=c("wpESPN_scores", "wpHamming_scores")) %>%
  rename(scoring_function = name) %>%
  rename(wp = value) %>%
  mutate(scoring_function = ifelse(scoring_function == "wpESPN_scores", "ESPN", "Hamming")) %>%
  group_by(n,k,scoring_function,strat) %>%
  mutate(max_wp = ifelse(wp==max(wp), wp, NA) ) %>%
  group_by(n,k,scoring_function,) %>%
  mutate(max_wp_1 = ifelse(wp==max(wp), wp, NA) ) %>%
  ungroup()
plot_df_1

#FIXME # changed this for the paper...
plot_df_1 = plot_df_1 %>% mutate(strat = ifelse(strat == 3, 2, strat))

my_palette_h_wp = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

#######################################
### Plot Expected Max Hamming Score ###
#######################################

for (strat_ in unique(plot_df_1$strat)) {
  for (opp_ in unique(plot_df_1$opp_prob_method)) {
    for (k_ in unique(plot_df_1$k)) {
      opp_str = case_when(
        opp_ == "naive_chalky" ~ "naively chalky",
        TRUE ~ opp_
      )
      pltttt = 
        plot_df_1 %>%
        filter(k == k_) %>%
        filter(opp_prob_method == opp_) %>%
        filter(strat == strat_) %>%
        mutate(
          # n_ = paste0("n = ", n),
          n_ = paste0(n),
          k_ = paste0(k),
          scoring_function = factor(scoring_function, levels=c("Hamming","ESPN"))
        ) %>%
        # filter(n >= 100) %>% #FIXME
        ggplot(aes(color = n_, x = lambda, y = wp)) + 
        facet_wrap(~scoring_function, scales="free_y") +
        geom_line(linewidth=1) +
        xlab(TeX("$\\lambda$")) +
        ylab("win probability") +
        # labs(title=TeX(paste0("$\\lambda$-strategy ", strat_))) +
        # labs(title=paste0("k = ", k_, ", ", opp_, " opponents")) +
        labs(title=TeX(paste0("k = ", k_, ", ", opp_str, " opponents, ", "$\\lambda$-strategy ", strat_))) +
        scale_color_manual(name="n", values=my_palette_h_wp) +
        geom_point(aes(y=max_wp), shape=21, size=2.5, stroke=0.5)
      pltttt
      ggsave(paste0(output_folder, "plot_fmm_chalkyLambda_wp_k=",k_,"_opp=",opp_,"_strat=",strat_,".png"),
             width=12, height=5)
    }
  }
}


#############################################################
### Which strategy is the best at each (n,k,ScoreFunc) ?? ###
#############################################################

df1 = bind_rows(
  plot_df %>% mutate(strat = paste0("lambda_strategy_",strat)),
  read_csv(paste0("plot_13/", "plot_grid_wp_fmm_v1_fold_1.csv")) %>% 
    # select(-c(entropy_cutoff,left_tail)) %>% 
    mutate(strat = "entropy_tail"),
)

df2 = df1 %>%
  filter(n >= 100) %>%
  group_by(n,k,strat) %>%
  summarise(
    wpHamming_score = max(wpHamming_scores),
    wpESPN_score    = max(wpESPN_scores),
    .groups = "drop"
  )
df2


df_strats_wpHamming_score = spread(df2 %>% select(-wpESPN_score),  strat, wpHamming_score)
df_strats_wpHamming_score

df_strats_wpESPN_score    = spread(df2 %>% select(-wpHamming_score),  strat, wpESPN_score)
df_strats_wpESPN_score


