
source("a2_main.R")

####################################################
### Bracket Diversity is Low for Chalky Brackets ###
####################################################

n = 500

bracket_set_naiveChalky = sample_n_brackets(n=n, prob_method = "naive_chalky")
bracket_set_random = sample_n_brackets(n=n)

bdnc = compute_bracket_diversity(bracket_set_naiveChalky)
bdr = compute_bracket_diversity(bracket_set_random)

bdnc
bdr

######################################################
### Enforce Bracket Diversity via Greedy Algorithm ###
######################################################

n = 500

bracket_set_greedy = sample_n_greedy_diverse_brackets(n=n, keep_probs=T) 
bracket_set_random = sample_n_brackets(n=n, keep_probs=T)
bracket_set_true = sample_n_brackets(n=250, keep_probs=T)

bdg = compute_bracket_diversity(bracket_set_greedy)
bdr = compute_bracket_diversity(bracket_set_random)

sg = compute_max_score(bracket_set_greedy, bracket_set_true, scoring_method="ESPN", expected_score=T) 
sr = compute_max_score(bracket_set_random, bracket_set_true, scoring_method="ESPN", expected_score=T) 

hg = compute_entropies(bracket_set_greedy)
hr = compute_entropies(bracket_set_random)

bdg
bdr
sg
sr
mean(hg)
mean(hr)

table(bracket_set_greedy$rd6_n)
table(bracket_set_random$rd6_n)
table(bracket_set_true$rd6_n)


