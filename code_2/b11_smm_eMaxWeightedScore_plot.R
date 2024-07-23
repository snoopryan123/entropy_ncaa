

source("b11_smm_main.R")

plot_df2 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v2.csv")) 

plot_df2 = plot_df2 %>%
  group_by(q_cutoff,n) %>%
  mutate(max_score = ifelse(eMaxScore == max(eMaxScore), eMaxScore, NA)) %>%
  ungroup()

# plot_df2 = plot_df2 %>% filter(n <= 100)

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


# # q_cutoff_ = 4.5
# for (q_cutoff_ in seq(1.5,5.5,by=1)) {
#   
#   plot_df2qc = plot_df2 %>% filter(q_cutoff == q_cutoff_)
#   
#   # title_ = paste0(
#   #   paste0("p = ", unique(plot_df2qc$p), "\n"),
#   #   paste0("q",(1:6)[1:6 < unique(plot_df2qc$q_cutoff)], " = qE", collapse=", "),
#   #   ", ",
#   #   paste0("q",(1:6)[1:6 > unique(plot_df2qc$q_cutoff)], " = qL", collapse=", "),
#   #   collapse=""
#   # )
#   
#   title_ = paste0(
#     # paste0("p = ", unique(plot_df2qc$p), "\n"),
#     paste0("p = ", unique(plot_df2qc$p), ",  "),
#     paste0(
#       paste0("q",(1:6)[1:6 < unique(plot_df2qc$q_cutoff)], collapse="="), "=qE,  "
#     ),
#     paste0(
#       paste0("q",(1:6)[1:6 > unique(plot_df2qc$q_cutoff)], collapse="="), "=qL"
#     )
#   )
#   
#   plot_eMaxWeightedScoreSmm = 
#     plot_df2qc %>%
#     mutate(
#       n_ = paste0("n = ", n),
#     ) %>%
#     ggplot(aes(x=qE,color=factor(qL),y=eMaxScore)) +
#     facet_wrap(~fct_reorder(n_,n)) +
#     geom_line(linewidth=1) +
#     geom_point(aes(y = max_score), size=2, shape=21, stroke=1.5) +
#     # geom_line(data = . %>% filter(p==qL & p==qE), color="green", linewidth=1.5) +
#     scale_color_manual(name="qL", values=my_palette_npq_v0) +
#     theme(panel.spacing = unit(2, "lines")) +
#     labs(title = title_) +
#     ylab("expected max Hamming score")
#   plot_eMaxWeightedScoreSmm
#   ggsave(paste0(output_folder,"plot_eMaxWeightedScoreSmm_qc",q_cutoff_,".png"),
#          width=12, height=6)
# }


################################################################################


plot_eMaxWeightedScoreSmm = 
  plot_df2 %>% 
  # filter(n %in% c(1,10,100,10000)) %>%
  filter(n %in% c(1,10,100)) %>%
  group_by(q_cutoff) %>%
  mutate(
    n_ = paste0("n=", n, ",  "),
    qE_str = paste0(
      paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
    ),
    qL_str = paste0(
      paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL"
    ),
    nq_ = paste0(n_, qE_str, qL_str)
  ) %>%
  ungroup() %>%
  ggplot(aes(x=qE,color=factor(qL),y=eMaxScore)) +
  # facet_wrap(~fct_reorder(nq_,q_cutoff), ncol=4) +
  facet_wrap(~fct_reorder(nq_,q_cutoff), ncol=3) +
  geom_line(linewidth=1) +
  geom_point(aes(y = max_score), size=2, shape=21, stroke=1.5) +
  scale_color_manual(name="qL", values=my_palette_npq_v0) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = paste0("p = ", unique(plot_df2qc$p))) +
  theme(text = element_text(size=40)) +
  theme(strip.text.x = element_text(size = 18)) +
  ylab("expected max ESPN score")
# plot_eMaxWeightedScoreSmm
ggsave(paste0(output_folder,"plot_eMaxWeightedScoreSmm",".png"),
       height=15, width=18 #24
       )
