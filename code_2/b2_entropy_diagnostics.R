
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

plot_entropy_hist(entropies, "random_1m", title="1 million randomly sampled brackets",savePlot=T)

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
typical_set_log2_size_bounds(0.3)


typical_set_prob_to_epsilon(.99)
typical_set_prob_to_epsilon(.95)
typical_set_prob_to_epsilon(.9)
typical_set_prob_to_epsilon(.8)
typical_set_prob_to_epsilon(.3)

#############################################
### Entropy equivalent to iid Ber(p) case ###
#############################################

### 
mean(entropies)
H_ = 48.7
H_/63

entropy_Ber_p = function(p) { -p*log(p,base=2) - (1-p)*log(1-p,base=2) }
entropy_Ber_p

# p = uniroot(function(p) entropy_Ber_p(p) - H_/63, lower=0.5, upper=0.999)$root
p = uniroot(function(p) 63*entropy_Ber_p(p) - H_, lower=0.5, upper=0.999)$root

p

entropy_Ber_p(p)
p






