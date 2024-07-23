
source("a2_main.R")
output_folder = "./plot_13/"

plot_df = read_csv(paste0(output_folder, "plot_grid_eMaxScore_fmm_v1_fold_1.csv"))
plot_df_1 = plot_df %>% 
  pivot_longer(cols=c("eMaxESPN_scores", "eMaxHamming_scores")) %>%
  rename(scoring_function = name) %>%
  rename(eMaxScore = value) %>%
  mutate(scoring_function = ifelse(scoring_function == "eMaxESPN_scores", "ESPN", "Hamming")) %>%
  group_by(n,scoring_function) %>%
  mutate(max_score = ifelse(eMaxScore==max(eMaxScore), eMaxScore, NA) ) %>%
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

plot_fmm_entropyTail_eMaxScore = 
  plot_df_1 %>%
  mutate(
    # n_ = paste0("n = ", n),
    n_ = paste0(n),
    scoring_function = factor(scoring_function, levels=c("Hamming","ESPN"))
  ) %>%
  # filter(n >= 10) %>% #FIXME
  filter(n >= 100) %>% #FIXME
  ggplot(aes(color = n_, x = h, y = eMaxScore)) + 
  facet_wrap(~scoring_function, scales="free_y") +
  geom_vline(xintercept = 48.7, color="gray60", linetype="dashed") +
  geom_line(linewidth=1) +
  ylab("expected maximum score") +
  scale_color_manual(name="n", values=my_palette_h_eMaxScore) +
  geom_point(aes(y=max_score), shape=21, size=2.5, stroke=0.5)
# plot_fmm_entropyTail_eMaxScore
ggsave(paste0(output_folder, "plot_fmm_entropyTail_eMaxScore.png"),
       width=12, height=5)

