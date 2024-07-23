
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

### takes 2 mins
hUs = c(40:60) #40:58
ns = 10^(2:3)
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

plot_espn_escore_vs_hU = as_tibble(scores_espn) %>%
  mutate(hU = rownames(scores_espn)) %>%
  pivot_longer(c("100","1000")) %>%
  rename(n=name, escore=value) %>%
  mutate(hU = as.numeric(hU)) %>%
  # select("Inf", "hU") %>%
  ggplot() +
  xlab(TeX("$h_U$")) +
  ylab("expected max ESPN score") +
  geom_vline(aes(xintercept=48.7), color="gray50", linetype="dashed") +
  scale_color_manual(name="n", values=c("firebrick", "firebrick2")) +
  geom_line(aes(color=factor(n), x=hU, y=escore), linewidth=1) 
plot_espn_escore_vs_hU
ggsave("plot_espn_escore_vs_hU.png", plot_espn_escore_vs_hU,
       width=8, height=5)


plot_num_correct_escore_vs_hU = as_tibble(scores_num_correct) %>%
  mutate(hU = rownames(scores_num_correct)) %>%
  pivot_longer(c("100","1000")) %>%
  rename(n=name, escore=value) %>%
  mutate(hU = as.numeric(hU)) %>%
  # select("Inf", "hU") %>%
  ggplot() +
  xlab(TeX("$h_U$")) +
  ylab("expected max num correct") +
  # geom_hline(aes(yintercept=48.7), color="gray50", linetype="dashed") +
  geom_vline(aes(xintercept=48.7), color="gray50", linetype="dashed") +
  scale_color_manual(name="n", values=c("firebrick", "firebrick2")) +
  geom_line(aes(color=factor(n), x=hU, y=escore), linewidth=1) 
plot_num_correct_escore_vs_hU
ggsave("plot_num_correct_escore_vs_hU.png", plot_num_correct_escore_vs_hU,
       width=8, height=5)

####################################
### Optimal Entropy Range Cutoff ###
####################################


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



############################################
### Brackets with certain Entropy Ranges ###
############################################

bracket_set_true = sample_n_brackets(n=250)
n = 1000

hUs = c(38:58,63) #40:58
widths = c(2,3,4,5,Inf) #3
scores_espn_A = matrix(nrow=length(hUs), ncol=length(widths))
rownames(scores_espn_A) = hUs
colnames(scores_espn_A) = widths
scores_num_correct_A = scores_espn_A
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
    scores_espn_A[i,j] = compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="ESPN", expected_score=T)
    scores_num_correct_A[i,j] = compute_max_score(bracket_set_ij, bracket_set_true, scoring_method="num_correct", expected_score=T)
  }
}

compute_bracket_diversity(bracket_set_ij)

####### visualize results

plot_espn_score = reshape2::melt(scores_espn_A, value.name="score") %>%
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

plot_num_correct_score = reshape2::melt(scores_num_correct_A, value.name="score") %>%
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



