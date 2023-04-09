
source("b8_simplified_MM_espn_main.R")
source("bb_plotting_colors.R")

######################
### Plotting Grids ###
######################

plot_grid_npq_v1 = read_csv("df_plot_grid_npq_v1.csv")

my_palette_npq_v1 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), #[3:9]
  "gray50",
  # rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:8]#[3:9]
)

# my_palette_npq_v1 = c(
#   rev(brewer.pal(name="Reds",n=9)[3:9]), #[3:9]
#   rev(brewer.pal(name="Purples",n=9)[4:8]), #[3:9]
#   "gray50",
#   # rev(brewer.pal(name="PuRd",n=9)[4:9]),
#   brewer.pal(name="Blues",n=9)[3:9]#[3:9]
# )


scores_ = c("Hamming", "ESPN")
for (i in 1:length(scores_)) {
  score_ = scores_[i]
  plot_simpMM_eMaxScore_npq = plot_grid_npq_v1 %>%
    # filter(0.2 <= q) %>%
    mutate(
      n_ = paste0("n = ", n),
      q = factor(q)
    ) %>%
    # ggplot(aes(x=p,color=q,y=eMaxScores_hamming)) +
    ggplot(aes_string(x="p", color="q", y=paste0("eMaxScores_", score_)) ) +
    facet_wrap(~n_) +
    geom_line(linewidth=1) +
    geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
    scale_color_manual(name="q", values=my_palette_npq_v1) +
    theme(panel.spacing = unit(2, "lines")) +
    # geom_point() +
    ylab(paste0("expected max ",score_," score"))
  # plot_simpMM_eMaxScore_npq_Hamming
  ggsave(paste0("plots/plot_simpMM_eMaxScore_npq_",score_,".png"),
         plot_simpMM_eMaxScore_npq,
         width=12,height=8)
  
  p_ = 0.7
  plot_simpMM_eMaxScore_npq1 = plot_grid_npq_v1 %>%
    filter(p == p_) %>%
    # filter(0.2 <= q) %>%
    mutate(
      n_ = paste0("n = ", n),
      # q = factor(q)
    ) %>%
    # ggplot(aes(x=p,color=q,y=eMaxScores_hamming)) +
    ggplot(aes_string(x="q", color="n_", y=paste0("eMaxScores_", score_)) ) +
    # facet_wrap(~n_) +
    geom_line(linewidth=1) +
    labs(title = paste0("p = ", p_)) +
    geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
    scale_color_manual(name="n", values=my_palette_npq_v1) +
    theme(panel.spacing = unit(2, "lines")) +
    # geom_point() +
    ylab(paste0("expected max ",score_," score"))
  plot_simpMM_eMaxScore_npq1
  ggsave(paste0("plots/plot_simpMM_eMaxScore_npq1_",score_,".png"),
         plot_simpMM_eMaxScore_npq1,
         width=12,height=8)
}





