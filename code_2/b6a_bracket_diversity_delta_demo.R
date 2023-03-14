
source("a2_main.R")

bracket_set_true = sample_n_brackets(n=250, keep_probs=T)

##################################
### Delta Diverse Bracket Sets ###
##################################

sample_n_delta_diverse_brackets(n=100, n0=10000, delta=2, 
  prob_method="P_538_2022", keep_probs=FALSE, print_every_n=50, entropy_range=NULL) 


ns = 10^(2:3)
deltas = 0:20
scores_espn = matrix(0, nrow=length(ns), ncol=length(deltas))
rownames(scores_espn) = ns
colnames(scores_espn) = deltas
scores_espn_R = scores_espn
scores_espn_E = scores_espn
scores_num_correct_R = scores_espn
scores_num_correct_E = scores_espn
num_runs = 3#15
for (M in 1:num_runs) {
  for (i in 1:length(ns)) {
    for (j in 1:length(deltas)) {
      n = ns[i]
      delta = deltas[j]
      print(paste0("run = ", M, ", n = ", n, ", delta = ", delta))
      
      stop("use optimal entropy range from another section")
      bracket_set_ijE = sample_n_delta_diverse_brackets(
        n=n, n0=10000, delta=delta, entropy_range=c(47,51), print_every_n = 1000
      )
      bracket_set_ijR = sample_n_delta_diverse_brackets(
        n=n, n0=10000, delta=delta, print_every_n = 1000
      )
      
      scores_espn_E[i,j] = compute_max_score(bracket_set_ijE, bracket_set_true, scoring_method="ESPN", expected_score=T)/num_runs
      scores_espn_R[i,j] = compute_max_score(bracket_set_ijR, bracket_set_true, scoring_method="ESPN", expected_score=T) /num_runs
      
      scores_num_correct_E[i,j] = compute_max_score(bracket_set_ijE, bracket_set_true, scoring_method="num_correct", expected_score=T)/num_runs 
      scores_num_correct_R[i,j] = compute_max_score(bracket_set_ijR, bracket_set_true, scoring_method="num_correct", expected_score=T)/num_runs
    }
  }
}





