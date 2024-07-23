
source("b11_smm_main.R")

plot_df = read_csv(paste0(output_folder,"plot_grid_eMaxHammingScore.csv"))
# plot_df = 
#   read_csv(paste0(output_folder,"plot_grid_eMaxScore_v1.csv")) %>% 
#   mutate(method="eWeightedMaxScore") %>%
#   mutate(p = p1, q = q1) %>%
#   rename(eMaxHammingScore = eMaxScore)

my_palette_npq_v0 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), 
  "gray50",
  brewer.pal(name="Blues",n=9)[3:8]
)

####################################################################
### Plot expected max Hamming score in Simplified March Madness: ###
### n submitted Ber(q) brackets vs. 1 true Ber(p) bracket        ###
####################################################################

for (method_ in unique(plot_df$method)) {
  # method_ = "pb"
  plot_eMaxHammingScoreSmm = 
    plot_df %>%
    filter(method==method_) %>%
    mutate(
      n_ = paste0("n = ", format_comma(n)),
      q = factor(q),
    ) %>%
    ggplot(aes(x=p,color=q,y=eMaxHammingScore)) +
    facet_wrap(~fct_reorder(n_,n)) +
    geom_line(linewidth=1) +
    geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
    scale_color_manual(name="q", values=my_palette_npq_v0) +
    theme(panel.spacing = unit(2, "lines")) +
    ylab("expected max Hamming score")
  plot_eMaxHammingScoreSmm
  ggsave(paste0(output_folder,"plot_eMaxHammingScoreSmm_",method_,".png"),
         width=12, height=6)
}

