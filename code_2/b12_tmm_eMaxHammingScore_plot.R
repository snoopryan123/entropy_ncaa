
source("b12_tmm_main.R")

plot_df_tmmEHam1 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v",version_,".csv"))

my_palette_npq_v0 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), 
  "gray50",
  brewer.pal(name="Blues",n=9)[3:8]
)

plot_eMaxHammingScoreSmm = 
  plot_df_tmmEHam1 %>%
  mutate(
    n_ = paste0("n = ", n),
    q = factor(q),
  ) %>%
  ggplot(aes(x=p,color=q,y=eMaxHammingScore)) +
  facet_wrap(~fct_reorder(n_,n)) +
  geom_line(linewidth=1) +
  geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
  scale_color_manual(name="q", values=my_palette_npq_v0) +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("expected max Hamming score")
# plot_eMaxHammingScoreSmm
ggsave(paste0(output_folder,"plot_eMaxHammingScoreTmm_v1",".png"),
       width=12, height=6)


