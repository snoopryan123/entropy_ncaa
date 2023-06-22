
source("a2_main.R")
output_folder = "./plot_14/"

plot_df = bind_rows(
  read_csv(paste0(output_folder, "plot_grid_eMaxScore_fmm_chalkyLambda_v1_fold_1.csv")),
  read_csv(paste0(output_folder, "plot_grid_eMaxScore_fmm_chalkyLambda_v1_fold_2.csv"))
)
  
plot_df_1 = plot_df %>% 
  pivot_longer(cols=c("eMaxESPN_scores", "eMaxHamming_scores")) %>%
  rename(scoring_function = name) %>%
  rename(eMaxScore = value) %>%
  mutate(scoring_function = ifelse(scoring_function == "eMaxESPN_scores", "ESPN", "Hamming")) %>%
  group_by(n,scoring_function,strat) %>%
  mutate(max_score = ifelse(eMaxScore==max(eMaxScore), eMaxScore, NA) ) %>%
  group_by(n,scoring_function,) %>%
  mutate(max_score_1 = ifelse(eMaxScore==max(eMaxScore), eMaxScore, NA) ) %>%
  ungroup()

my_palette_h_eMaxScore = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

#######################################
### Plot Expected Max Hamming Score ###
#######################################

for (strat_ in unique(plot_df_1$strat)) {
  
plot_fmm_chalkyLambda_eMaxScore = 
  plot_df_1 %>%
  mutate(
    n_ = paste0(n),
    scoring_function = factor(scoring_function, levels=c("Hamming","ESPN")),
    # strat_score = paste0(scoring_function, " Score, Strategy ", strat)
  ) %>%
  # filter(n >= 10) %>% #FIXME
  filter(n >= 100) %>% #FIXME
  filter(strat == strat_) %>%
  ggplot(aes(color = n_, x = lambda, y = eMaxScore)) + 
  facet_wrap(~scoring_function, scales="free_y") +
  # facet_wrap(~strat_score, scales="free_y") +
  geom_line(linewidth=1) +
  xlab(TeX("$\\lambda$")) +
  ylab("expected maximum score") +
  labs(title=TeX(paste0("$\\lambda$-strategy ", strat_))) +
  scale_color_manual(name="n", values=my_palette_h_eMaxScore) +
  # geom_point(aes(y=max_score_1), shape=17, size=2.5, stroke=0.5) +
  geom_point(aes(y=max_score), shape=21, size=2.5, stroke=0.5)
# plot_fmm_chalkyLambda_eMaxScore
ggsave(paste0(output_folder, "plot_fmm_chalkyLambda_eMaxScore_s",strat_,".png"),
       width=12, height=5)

}


###########################################################
### Which strategy is the best at each (n,ScoreFunc) ?? ###
###########################################################

df1 = bind_rows(
  plot_df %>% mutate(strat = paste0("lambda_strategy_",strat)),
  read_csv(paste0("plot_13/", "plot_grid_eMaxScore_fmm_v1_fold_1.csv")) %>% 
    # select(-c(entropy_cutoff,left_tail)) %>% 
    mutate(strat = "entropy_tail"),
)

df2 = df1 %>%
  filter(n >= 100) %>%
  group_by(n,strat) %>%
  summarise(
    eMaxHamming_score = max(eMaxHamming_scores),
    eMaxESPN_score = max(eMaxESPN_scores),
    .groups = "drop"
  )
df2


df_strats_eMaxHamming_score = spread(df2 %>% select(-eMaxESPN_score),  strat, eMaxHamming_score)
df_strats_eMaxHamming_score

df_strats_eMaxESPN_score    = spread(df2 %>% select(-eMaxHamming_score),  strat, eMaxESPN_score)
df_strats_eMaxESPN_score


