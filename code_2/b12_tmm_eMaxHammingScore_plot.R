
source("b12_tmm_main.R")

###############################################################
### Plot Expected Max Hamming Score for Grid 1 (constant q) ###
###############################################################

plot_df_tmmEHam1 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v1.csv"))
plot_df_tmmEHam1 = plot_df_tmmEHam1 %>%
  group_by(p,q,n,scoring_method) %>%
  mutate(eMaxHammingScore = mean(eMaxHammingScore)) %>%
  ungroup() %>%
  filter(fold == 1) 

my_palette_npq_v0 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), 
  "gray50",
  brewer.pal(name="Blues",n=9)[3:8]
)

plot_eMaxHammingScoreTmm = 
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
# plot_eMaxHammingScoreTmm
ggsave(paste0(output_folder,"plot_eMaxHammingScoreTmm_v1",".png"),
       width=12, height=6)

###########################################################
### Plot Expected Max Hamming Score for Grid 2 (qE, qL) ###
###########################################################

plot_df_tmmEHam2 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v2.csv"))
plot_df_tmmEHam2 = plot_df_tmmEHam2 %>%
  group_by(p,qE,qL,q_cutoff,n,scoring_method) %>%
  mutate(eMaxHammingScore = mean(eMaxHammingScore)) %>%
  ungroup() %>%
  filter(fold == 1) 
plot_df_tmmEHam2 = plot_df_tmmEHam2 %>%
  group_by(q_cutoff,n) %>%
  mutate(max_score = ifelse(eMaxHammingScore == max(eMaxHammingScore), eMaxHammingScore, NA)) %>%
  ungroup()

plot_eMaxHammingScoreTmm_2 = 
  plot_df_tmmEHam2 %>% 
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
  ggplot(aes(x=qE,color=factor(qL),y=eMaxHammingScore)) +
  # facet_wrap(~fct_reorder(nq_,q_cutoff), ncol=4) +
  facet_wrap(~fct_reorder(nq_,q_cutoff), ncol=3) +
  geom_line(linewidth=1) +
  geom_point(aes(y = max_score), size=2, shape=21, stroke=1.5) +
  scale_color_manual(name="qL", values=my_palette_npq_v0) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = paste0("p = ", unique(plot_df_tmmEHam2$p))) +
  theme(text = element_text(size=40)) +
  theme(strip.text.x = element_text(size = 18)) +
  ylab("expected max Hamming score")
# plot_eMaxWeightedScoreSmm
ggsave(paste0(output_folder,"plot_eMaxHammingScoreTmm_v2",".png"),
       width=20, height=18)


###########################################################
### Plot Expected Max ESPN Score for Grid 2 (qE, qL) ###
###########################################################

plot_df_tmmESPN3 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v3.csv"))
plot_df_tmmESPN3 = plot_df_tmmESPN3 %>%
  group_by(p,qE,qL,q_cutoff,n,scoring_method) %>%
  mutate(eMaxScore = mean(eMaxScore)) %>%
  ungroup() %>%
  filter(fold == 1) 
plot_df_tmmESPN3 = plot_df_tmmESPN3 %>%
  group_by(q_cutoff,n) %>%
  mutate(max_score = ifelse(eMaxScore == max(eMaxScore), eMaxScore, NA)) %>%
  ungroup()

plot_eMaxESPNScoreTmm_3 = 
  plot_df_tmmESPN3 %>% 
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
  geom_point(aes(y = max_score), size=3, shape=31, stroke=1.5) +
  scale_color_manual(name="qL", values=my_palette_npq_v0) +
  theme(panel.spacing = unit(3, "lines")) +
  labs(title = paste0("p = ", unique(plot_df_tmmESPN3$p))) +
  theme(text = element_text(size=40)) +
  theme(strip.text.x = element_text(size = 18)) +
  ylab("expected max ESPN score")
# plot_eMaxWeightedScoreSmm
ggsave(paste0(output_folder,"plot_eMaxESPNScoreTmm_v3",".png"),
       width=30, height=18)





