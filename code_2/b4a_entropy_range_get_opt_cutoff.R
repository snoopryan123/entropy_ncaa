
source("a2_main.R")

###########################################################################
### Optimal Entropy Range Cutoff: Sample from Left half of entropy dist ###
###########################################################################

set.seed(99)
bracket_set_true = sample_n_brackets(n=250)

### takes 10 mins
hUs = c(38:63) #c(40:60) #40:58
ns = 10^(0:4)
scores_espn = matrix(0, nrow=length(hUs), ncol=length(ns))
# rownames(scores_espn) = paste0("hU",hUs)
# colnames(scores_espn) = paste0("n",ns)
rownames(scores_espn) = hUs
colnames(scores_espn) = ns
scores_num_correct = scores_espn
num_runs = 15
for (M in 1:num_runs) {
  for (i in 1:length(hUs)) {
    for (j in 1:length(ns)) {
      print(c(M,i,j))
      hU = hUs[i]
      n = ns[j]
      bracket_set_ij = sample_n_brackets_entropyRange(n, -Inf, hU)
      entropies_ij = compute_entropies(bracket_set_ij)
      # c(min(entropies_ij), max(entropies_ij))
      scores_espn[i,j] = scores_espn[i,j] + 
        compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="ESPN", expected_score=T)/num_runs
      scores_num_correct[i,j] = scores_num_correct[i,j] + 
        compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="num_correct", expected_score=T)/num_runs
    }
  }
}

write.csv(scores_espn, "df_scores_espn_b4a.csv")
write.csv(scores_num_correct, "df_scores_num_correct_b4a.csv")

####################################
### Optimal Entropy Range Cutoff ###
####################################




