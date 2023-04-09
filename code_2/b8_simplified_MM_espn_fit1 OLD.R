
source("b8_simplified_MM_espn_main.R")

######################
### Plotting Grids ###
######################

plot_grid_npq_v1 = tibble(expand.grid(
  # p = seq(0.5, 1, by=0.05),
  # q = seq(0.05, 1, by=0.05),
  p = seq(0.5, 1, by=0.1),
  q = seq(0.1, 1, by=0.1),
  n = 10^(0:8)
  # n = 10^(seq(0,16,by=2))
))
plot_grid_npq_v1

eMaxScores_v1_hamming = numeric(nrow(plot_grid_npq_v1))
eMaxScores_v1_espn = numeric(nrow(plot_grid_npq_v1))
for (i in 1:nrow(plot_grid_npq_v1)) {
  if (i %% 50 == 0) print(i)
  p = plot_grid_npq_v1$p[i]
  q = plot_grid_npq_v1$q[i]
  n = plot_grid_npq_v1$n[i]
  p_vec = rep(p,m)
  q_vec = rep(q,m)
  eMaxScores_v1_hamming[i] = eMaxScore(n=n, p_vec=p_vec, q_vec=q_vec, score="Hamming")
  eMaxScores_v1_espn[i] = eMaxScore(n=n, p_vec=p_vec, q_vec=q_vec, score="ESPN")
}
plot_grid_npq_v1$eMaxScores_Hamming = eMaxScores_v1_hamming
plot_grid_npq_v1$eMaxScores_ESPN = eMaxScores_v1_espn
write_csv(plot_grid_npq_v1, "df_plot_grid_npq_v1.csv")



