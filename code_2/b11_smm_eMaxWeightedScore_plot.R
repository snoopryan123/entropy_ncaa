

source("b11_smm_main.R")

plot_df2 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v2.csv")) 

plot_df2 = plot_df2 %>% filter(n <= 100)

my_palette_npq_v0 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), 
  "gray50",
  brewer.pal(name="Blues",n=9)[3:8]
)

####################################################################
### Plot expected max Hamming score in Simplified March Madness: ###
### n submitted Ber(q) brackets vs. 1 true Ber(p) bracket        ###
####################################################################

# plot_eMaxWeightedScoreSmm = 
#   plot_df2 %>%
#   filter(n == 10000000) %>%
#   mutate(
#     n_ = paste0("n = ", n),
#   ) %>%
#   ggplot(aes(x=qE,y=qL,fill=eMaxScore)) +
#   facet_wrap(~fct_reorder(n_,n)) +
#   geom_tile() +
#   scale_fill_gradientn(name="expected\nmax\nESPN\nscore", colours = rev(terrain.colors(7))) +
#   theme(panel.spacing = unit(2, "lines")) +
#   geom_hline(yintercept = 0.75, color="gray60", linetype="dashed", linewidth=1)
# plot_eMaxWeightedScoreSmm
# 
#   # geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
#   scale_color_manual(name="q", values=my_palette_npq_v0) +
#   ylab("expected max Hamming score")
# plot_eMaxWeightedScoreSmm



plot_eMaxWeightedScoreSmm = 
  plot_df2 %>%
  mutate(
    n_ = paste0("n = ", n),
  ) %>%
  ggplot(aes(x=qE,color=factor(qL),y=eMaxScore)) +
  facet_wrap(~fct_reorder(n_,n)) +
  geom_line(linewidth=1) +
  # geom_line(data = . %>% filter(p==qL & p==qE), color="green", linewidth=1.5) +
  scale_color_manual(name="qL", values=my_palette_npq_v0) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = paste0("p = ", unique(plot_df2$p))) +
  ylab("expected max Hamming score")
plot_eMaxWeightedScoreSmm
ggsave(paste0(output_folder,"plot_eMaxWeightedScoreSmm_",method_,".png"),
       width=12, height=6)


