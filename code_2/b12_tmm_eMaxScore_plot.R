
source("b12_tmm_main.R")

my_palette_npq_v0 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), 
  "gray50",
  brewer.pal(name="Blues",n=9)[3:8]
)

###############################################################
### Plot Expected Max Hamming Score for Grid 1 (constant q) ###
###############################################################

plot_df_tmmEHam1 = read_csv(paste0(output_folder,"plot_grid_eMaxScore_v1.csv"))
plot_df_tmmEHam1 = plot_df_tmmEHam1 %>%
  group_by(p,q,n,scoring_method) %>%
  mutate(eMaxHammingScore = mean(eMaxHammingScore)) %>%
  ungroup() %>%
  filter(fold == 1) 

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

for (clipped_ in c(TRUE,FALSE)) {
  # clipped_ = TRUE

  plot_eMaxHammingScoreTmm_2 = 
    plot_df_tmmEHam2 %>% 
    # filter(n %in% c(1,10,100,10000)) %>%
    filter(n %in% c(1,10,100)) %>%
    mutate(clipped = clipped_) %>%
    filter(ifelse(clipped, 
                  q_cutoff < 4.5,
                  TRUE)) %>%
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
    labs(title = paste0("p = ", unique(plot_df_tmmEHam2$p))) +
    theme(
      plot.title = element_text(size = if (clipped_) 40 else 70),
      strip.text.x = element_text(size = if (clipped_) 19 else 18),
      axis.text.x = element_text(size = if (clipped_) 30 else 40),
      axis.text.y = element_text(size = if (clipped_) 30 else 40),
      axis.title.x = element_text(size = if (clipped_) 40 else 80),
      axis.title.y = element_text(size = if (clipped_) 40 else 80),
      legend.title = element_text(size = if (clipped_) 35 else 70), 
      legend.text  = element_text(size = if (clipped_) 35 else 50),
      legend.key.size = unit(if (clipped_) 1 else 2, "lines"),
      panel.spacing = unit(1.5, "lines")
    ) +
    ylab("expected max Hamming score")
    # plot_eMaxHammingScoreTmm_2
    ggsave(paste0(output_folder,"plot_eMaxHammingScoreTmm_v2",if (clipped_) "_clipped",".png"),
            width=if (clipped_) 18 else 20, height=if (clipped_) 12 else 18
           )
}

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


for (clipped_ in c(TRUE,FALSE)) {
  # clipped_ = TRUE
  
  plot_eMaxESPNScoreTmm_3 = 
    plot_df_tmmESPN3 %>% 
    # filter(n %in% c(1,10,100,10000)) %>%
    filter(n %in% c(1,10,100)) %>%
    mutate(clipped = clipped_) %>%
    filter(ifelse(clipped, 
                  q_cutoff < 4.5,
                  TRUE)) %>%
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
    labs(title = paste0("p = ", unique(plot_df_tmmESPN3$p))) +
    theme(
      plot.title = element_text(size = if (clipped_) 40 else 70),
      strip.text.x = element_text(size = if (clipped_) 19 else 18),
      axis.text.x = element_text(size = if (clipped_) 30 else 40),
      axis.text.y = element_text(size = if (clipped_) 30 else 40),
      axis.title.x = element_text(size = if (clipped_) 40 else 80),
      axis.title.y = element_text(size = if (clipped_) 40 else 80),
      legend.title = element_text(size = if (clipped_) 35 else 70), 
      legend.text  = element_text(size = if (clipped_) 35 else 50),
      legend.key.size = unit(if (clipped_) 1 else 2, "lines"),
      panel.spacing = unit(1.5, "lines")
    ) +
    ylab("expected max ESPN score")
  plot_eMaxESPNScoreTmm_3
  ggsave(paste0(output_folder,"plot_eMaxESPNScoreTmm_v3",if (clipped_) "_clipped",".png"),
         width=if (clipped_) 18 else 20, height=if (clipped_) 12 else 18
  )
}




