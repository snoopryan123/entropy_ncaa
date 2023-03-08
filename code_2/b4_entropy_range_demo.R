
source("a2_main.R")

### look at observed entropy dist
ex_bracket_set = sample_n_brackets(n=1e5, keep_probs=T)
ex_entropies = compute_entropies(ex_bracket_set)
mean(ex_entropies)
sd(ex_entropies)
# hist(ex_entropies)
quantile(ex_entropies, seq(0,1,length=19))

############################################
### Brackets with certain Entropy Ranges ###
############################################

bracket_set_true = sample_n_brackets(n=250)
n = 1000

hUs = c(38:58,63) #40:58
widths = c(2,3,4,5,Inf) #3
scores_espn = matrix(nrow=length(hUs), ncol=length(widths))
rownames(scores_espn) = hUs
colnames(scores_espn) = widths
scores_num_correct = scores_espn
for (i in 1:length(hUs)) {
  for (j in 1:length(widths)) {
    print(c(i,j))
    hU = hUs[i]
    hL = hU - widths[j]
    # hL = hLs[i]
    # hU = hL + widths[j]
    bracket_set_ij = sample_n_brackets_entropyRange(n, hL, hU)
    entropies_ij = compute_entropies(bracket_set_ij)
    # c(min(entropies_ij), max(entropies_ij))
    scores_espn[i,j] = compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="ESPN", expected_score=T)
    scores_num_correct[i,j] = compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="num_correct", expected_score=T)
  }
}

compute_bracket_diversity(bracket_set_ij)

####### visualize results

plot_espn_score = reshape2::melt(scores_espn, value.name="score") %>%
  rename(hU=Var1) %>% rename(width=Var2) %>%
  ggplot(aes(x=factor(width),y=factor(hU))) +
  geom_tile(aes(fill=score)) +
  scale_fill_gradient(low="grey80", high="darkred") +
  geom_text(aes(label=score), color="white") +
  # xlab("upper entropy limit hU")
  ylab(TeX("upper entropy limit $h_U$")) +
  xlab("entropy interval width") +
  guides(fill=guide_legend(title=" ESPN\n Score"))
# plot_espn_score
ggsave("plot_espn_score.png", plot_espn_score, width=10, height=13)

plot_num_correct_score = reshape2::melt(scores_num_correct, value.name="score") %>%
  rename(hU=Var1) %>% rename(width=Var2) %>%
  ggplot(aes(x=factor(width),y=factor(hU))) +
  geom_tile(aes(fill=score)) +
  scale_fill_gradient(low="grey80", high="darkred") +
  geom_text(aes(label=score), color="white") +
  # xlab("upper entropy limit hU")
  ylab(TeX("upper entropy limit $h_U$")) +
  xlab("entropy interval width") +
  guides(fill=guide_legend(title=" Num\n Correct"))
# plot_num_correct_score
ggsave("plot_num_correct_score.png", plot_num_correct_score, width=10, height=13)


###################################################
### Enforce Bracket Diversity and Entropy Range ###
###################################################

bracket_set_diverse = sample_n_greedy_diverse_brackets(n, entropy_range=c(47,51))

compute_max_score(bracket_set_diverse, bracket_set_true, scoring_method="ESPN", expected_score=T)
compute_max_score(bracket_set_diverse, bracket_set_true, scoring_method="num_correct", expected_score=T)



