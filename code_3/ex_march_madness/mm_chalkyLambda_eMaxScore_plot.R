
PROB_METHOD = "P_538_2022"
# PROB_METHOD = "P1"
output_folder = paste0("plots/plot_", PROB_METHOD, "_")

###################
filewd = getwd()
setwd("..")
source("a2_main.R")
setwd(filewd)
###################

# output_folder = "./plots/"
num_folds_ = 19

plot_df = tibble()
for (fold in 1:num_folds_) {
  df_fold = read_csv(paste0(output_folder, "plot_grid_eMaxScore_fmm_chalkyLambda_v1_fold_",fold,".csv"), show_col_types = F)
  df_fold$fold = fold
  plot_df = bind_rows(plot_df, df_fold)
}
  
plot_df_1 = 
  plot_df %>% 
  pivot_longer(cols=c("eMaxESPN_scores", "eMaxHamming_scores")) %>%
  rename(scoring_function = name) %>%
  rename(eMaxScore = value) %>%
  mutate(scoring_function = ifelse(scoring_function == "eMaxESPN_scores", "ESPN", "Hamming")) %>%
  group_by(n,scoring_function,strat) %>%
  mutate(max_score = ifelse(eMaxScore==max(eMaxScore), eMaxScore, NA) ) %>%
  group_by(n,scoring_function,) %>%
  mutate(max_score_1 = ifelse(eMaxScore==max(eMaxScore), eMaxScore, NA) ) %>%
  ungroup()
plot_df_1

my_palette_h_eMaxScore = c(
  brewer.pal(name="PuRd",n=9)[3:8]
  # rev(brewer.pal(name="Reds",n=9)[4:8]), 
  # "gray50",
  # brewer.pal(name="Blues",n=9)[3:8]
)

###############################
### Plot Expected Max Score ###
###############################

plot_fmm_chalkyLambda_eMaxScore = 
  plot_df_1 %>%
  filter(
    scoring_function == "ESPN",
    strat == 3,
  ) %>%
  filter(n %in% c(100, 500, 1000, 10000)) %>%
  mutate(n_ = paste0(format_comma(n)), n_=fct_reorder(n_, n)) %>%
  # filter(n >= 100) %>% #FIXME
  ggplot(aes(color = n_, x = lambda, y = eMaxScore)) + 
  geom_line(linewidth=1) +
  xlab(TeX("$\\lambda$")) +
  ylab("expected maximum ESPN score") +
  labs(color="n") +
  scale_color_manual(values=my_palette_h_eMaxScore) +
  scale_x_continuous(breaks=seq(0,1,by=0.1)) +
  geom_point(aes(y=max_score), shape=21, size=2.5, stroke=0.5)
# plot_fmm_chalkyLambda_eMaxScore
ggsave(paste0(output_folder, "plot_mm_chalkyLambda_eMaxScore.png"), width=10, height=6)


