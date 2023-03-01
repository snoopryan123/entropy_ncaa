
source("a2_main.R")

###########################################################################################
### How does sampling random brackets compare against a field of naive chalky brackets? ###
###########################################################################################

### ex
ex_true_backets = sample_n_brackets(n=250)

Nc = 1000
Nc = 1.73*1e7 ### 17.3 million
ex_submitted_backets_naiveChalky = sample_n_brackets(n=Nc, prob_method = "naive_chalky")
ex_scores_naiveChalky = compute_max_score(ex_submitted_backets_naiveChalky, ex_true_backets, scoring_method="ESPN")

ns = 10**(1:4)
ns = 10**(1:6)
scores = numeric(length(ns))
for (i in 1:length(ns)) {
  print(i)
  n = ns[i]
  ex_submitted_backets_elo = sample_n_brackets(n=n, prob_method = "P_538_2022")
  ex_scores_elo = compute_max_score(ex_submitted_backets_elo, ex_true_backets, scoring_method="ESPN")
  
  ex_score_diffs = ex_scores_elo - ex_scores_naiveChalky
  ex_score_diffs = ex_score_diffs[ex_score_diffs != 0]
  scores[i] = mean(ex_score_diffs > 0)
}

scores








