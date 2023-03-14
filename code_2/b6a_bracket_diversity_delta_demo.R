
source("a2_main.R")

bracket_set_true = sample_n_brackets(n=250, keep_probs=T)

##################################
### Delta Diverse Bracket Sets ###
##################################

hU <- function(n,scoring_method) { 
  if (n==100 & scoring_method=="ESPN") {
    return(46)
  } else if (n==1000 & scoring_method=="ESPN") {
    return(49)
  } else if (n==100 & scoring_method=="num_correct") {
    40 #38
  } else if (n==1000 & scoring_method=="num_correct") {
    return(48)
  } else {
    stop("need to implement this case for n and scoring_method")
  }
}
ns = 100 #10^(2:3)
deltas = 0:10 #4 #20
scores_espn = matrix(0, nrow=length(ns), ncol=length(deltas))
rownames(scores_espn) = ns
colnames(scores_espn) = deltas
scores_espn_R = scores_espn
scores_espn_E = scores_espn
scores_num_correct_R = scores_espn
scores_num_correct_E = scores_espn
num_runs = 10 #1 #3#15
for (M in 1:num_runs) {
  for (i in 1:length(ns)) {
    for (j in 1:length(deltas)) {
      n = ns[i]
      delta = deltas[j]
      print(paste0("run = ", M, ", n = ", n, ", delta = ", delta))
      
      bracket_set_ijE_num_correct = sample_n_delta_diverse_brackets(
        n=n, n0=10000, delta=delta, entropy_range=c(-Inf, hU(n,"num_correct")), print_every_n = 1000
      )
      bracket_set_ijE_espn = sample_n_delta_diverse_brackets(
        n=n, n0=10000, delta=delta, entropy_range=c(-Inf, hU(n,"ESPN")), print_every_n = 1000
      )
      bracket_set_ijR = sample_n_delta_diverse_brackets(
        n=n, n0=10000, delta=delta, print_every_n = 1000
      )
      
      scores_espn_E[i,j] = compute_max_score(bracket_set_ijE_espn, bracket_set_true, scoring_method="ESPN", expected_score=T)/num_runs
      scores_espn_R[i,j] = compute_max_score(bracket_set_ijR, bracket_set_true, scoring_method="ESPN", expected_score=T) /num_runs
      
      scores_num_correct_E[i,j] = compute_max_score(bracket_set_ijE_num_correct, bracket_set_true, scoring_method="num_correct", expected_score=T)/num_runs 
      scores_num_correct_R[i,j] = compute_max_score(bracket_set_ijR, bracket_set_true, scoring_method="num_correct", expected_score=T)/num_runs
    }
  }
}

write.csv(scores_espn_R, "df_scores_espn_R_b6a.csv")
write.csv(scores_espn_E, "df_scores_espn_E_b6a.csv")
write.csv(scores_num_correct_R, "df_scores_num_correct_R_b6a.csv")
write.csv(scores_num_correct_E, "df_scores_num_correct_E_b6a.csv")






