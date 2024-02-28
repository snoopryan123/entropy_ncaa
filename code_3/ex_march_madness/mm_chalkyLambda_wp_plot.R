
###################
filewd = getwd()
setwd("..")
source("a2_main.R")
setwd(filewd)
###################

output_folder = "./plots/"
num_folds_ = 19

plot_df = tibble()
for (fold in 1:num_folds_) {
  df_fold = read_csv(paste0(output_folder, "plot_grid_wp_fmm_chalkyLambda_v1_fold_",fold,".csv"), show_col_types = F)
  df_fold$fold = fold
  plot_df = bind_rows(plot_df, df_fold)
}

plot_df_1 = 
  plot_df %>% 
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

my_palette_h_wp = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

###############
### Plot WP ###
###############

k_ = 10000
opp_str = "colloquially chalky"
pltttt = 
  plot_df_1 %>%
  mutate(
    opp_prob_method = 
      ifelse(opp_prob_method == "naive_chalky", "colloquially chalky", opp_prob_method)
  ) %>%
  filter(
    k == k_,
    scoring_function == "ESPN",
    opp_prob_method == opp_str,
    strat == 3,
  ) %>%
  filter(n %in% c(100, 500, 1000, 10000)) %>%
  mutate(n_ = paste0(n), n_=fct_reorder(n_, n), k_ = paste0(k), k_=fct_reorder(k_, k),) %>%
  ggplot(aes(color = n_, x = lambda, y = wp)) + 
  geom_line(linewidth=1) +
  xlab(TeX("$\\lambda$")) +
  ylab("win probability") +
  scale_x_continuous(breaks=seq(0,1,by=0.1)) +
  labs(title=TeX(paste0("k = ", k_, " ", opp_str, " opponents"))) +
  scale_color_manual(name="n", values=my_palette_h_wp) +
  geom_point(aes(y=max_wp), shape=21, size=2.5, stroke=0.5)
# pltttt
ggsave(paste0(output_folder, "plot_mm_chalkyLambda_wp.png"), width=10, height=6)



