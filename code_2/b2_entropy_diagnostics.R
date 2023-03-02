
source("a2_main.R")

#########################################
### Entropy H and Entropic Variance V ###
#########################################

### ex
bracket_set = sample_n_brackets(n=1000000, keep_probs = TRUE)
entropies = compute_entropies(bracket_set)

mean(entropies)
var(entropies)
sd(entropies)

plot_entropy_hist(entropies, "random_100k", title="100k randomly sampled brackets",savePlot=T)

H_ = 48.7
V_ = 10.8

typical_set_prob_to_epsilon <- function(prob, H=H_, v=V_) {
  sqrt(v/(1-prob))
}

typical_set_log2_size_bounds <- function(prob, H=H_, v=V_) {
  epsilon = typical_set_prob_to_epsilon(prob, H=H, v=v)
  L = log(
    (1-v/epsilon^2)*2^(H-epsilon), 
    base=2
  )
  U = log(
    2^(H+epsilon), 
    base=2
  )
  c(L,U)
}

typical_set_log2_size_bounds(0.99)
typical_set_log2_size_bounds(0.95)
typical_set_log2_size_bounds(0.9)
typical_set_log2_size_bounds(0.8)

  
  






