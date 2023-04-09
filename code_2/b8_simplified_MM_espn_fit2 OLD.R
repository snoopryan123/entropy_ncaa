
source("b8_simplified_MM_espn_main.R")

######################
### Plotting Grids ###
######################

plot_grid_npq_v2 = tibble(expand.grid(
  # p = seq(0.5, 0.99, length=50),
  # q = seq(0.5, 0.99, length=50),
  # p = seq(0.5, 1, by=0.05),
  # q = seq(0.05, 1, by=0.05),
  # p = seq(0.5, 1, by=0.1),
  p = 0.75,
  q1 = seq(0.5, 1, by=0.1),
  q2 = seq(0.5, 1, by=0.1),
  q3 = seq(0.5, 1, by=0.1),
  q4 = seq(0.5, 1, by=0.1),
  q5 = seq(0.5, 1, by=0.1),
  q6 = seq(0.5, 1, by=0.1),
  # q1 = seq(0.5, 1, by=0.05),
  # q2 = seq(0.5, 1, by=0.05),
  # q3 = seq(0.5, 1, by=0.05),
  # q4 = seq(0.5, 1, by=0.05),
  # q5 = seq(0.5, 1, by=0.05),
  # q6 = seq(0.5, 1, by=0.05),
  n = 10^(0:8)
))
plot_grid_npq_v2

eMaxScores_v2_hamming = numeric(nrow(plot_grid_npq_v2))
eMaxScores_v2_espn = numeric(nrow(plot_grid_npq_v2))
for (i in 1:nrow(plot_grid_npq_v2)) {
  if (i %% 50 == 0) print(i)
  p = plot_grid_npq_v2$p[i]
  q1 = plot_grid_npq_v2$q1[i]
  q2 = plot_grid_npq_v2$q2[i]
  q3 = plot_grid_npq_v2$q3[i]
  q4 = plot_grid_npq_v2$q4[i]
  q5 = plot_grid_npq_v2$q5[i]
  q6 = plot_grid_npq_v2$q6[i]
  n = plot_grid_npq_v2$n[i]
  p_vec = rep(p,m)
  q_vec = flatten_dbl( sapply(1:R, function (r) rep( get(paste0("q",r)) , 2^(R-r)) ) )
  eMaxScores_v2_hamming[i] = eMaxScore(n=n, p_vec=p_vec, q_vec=q_vec, score="Hamming")
  eMaxScores_v2_espn[i] = eMaxScore(n=n, p_vec=p_vec, q_vec=q_vec, score="ESPN")
}
plot_grid_npq_v2$eMaxScores_hamming = eMaxScores_v2_hamming
plot_grid_npq_v2$eMaxScores_espn = eMaxScores_v2_espn

