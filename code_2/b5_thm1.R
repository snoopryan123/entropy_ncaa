
###################################
### Code for Simplied Theorem 1 ###
###################################

source("a2_main.R")

###############
### Entropy ###
###############

### ex
bracket_set = sample_n_brackets(n=1000000, keep_probs = TRUE)
entropies = compute_entropies(bracket_set)

mean(entropies)
H_ = 48.7
H_/63

entropy_Ber_p = function(p) { -p*log(p,base=2) - (1-p)*log(1-p,base=2) }
entropy_Ber_p

p = uniroot(function(p) entropy_Ber_p(p) - H_/63, lower=0.5, upper=0.999)$root

H_/63
entropy_Ber_p(p)
p

m = 63 ### number of games in the NCAA tournament

############################################
### Expected Score of One Random Bracket ###
############################################

emaxscore_random1 <- function(p) {
  m*(p^2 + (1-p)^2)
}
### check
# emaxscore_random1(0.5)
# emaxscore_random1(0.75)
# emaxscore_random1(0.9)
# emaxscore_random1(1)

############################################
### Expected Score of One Chalky Bracket ###
############################################

emaxscore_chalky1 <- function(p,u) {
  (63-u)*p + u*(1-p)
}
### check
# emaxscore_chalky1(0.5, 3)
# emaxscore_chalky1(0.5, 4)
# emaxscore_chalky1(0.75, 3)
# emaxscore_chalky1(0.75, 4)
# emaxscore_chalky1(0.9, 3)
# emaxscore_chalky1(0.9, 4)

emaxscore_chalky1_umax <- function(p,u_max=4) {
  vec = numeric(u_max+1)
  for (u in 0:u_max) {
    vec[u+1] = emaxscore_chalky1(p,u) * dbinom(u, size=m, prob=1-p)/pbinom(u_max, size=m, prob=1-p, lower.tail = TRUE)
  }
  sum(vec)
}
### check
# emaxscore_chalky1_umax(0.5, 3)
# emaxscore_chalky1_umax(0.5, 4)
# emaxscore_chalky1_umax(0.75, 3)
# emaxscore_chalky1_umax(0.75, 4)
# emaxscore_chalky1_umax(0.9, 3)
# emaxscore_chalky1_umax(0.9, 4)

##############################################
### Plot the Expected Score of One Bracket ###
##############################################

# color_vec =  c("chalky"="firebrick", "random"="dodgerblue2")
# name_vec1 = c("chalky"="one\nchalky\nbracket\n", "random"="one\nrandom\nbracket\n")
color_vec =  c(
  "chalky3"="firebrick2", 
  "chalky10"="firebrick", 
  "random"="dodgerblue2"
)
name_vec1 = c(
  "chalky3"="one\nchalky\nbracket\n(u_max=3)\n", 
  "chalky10"="one\nchalky\nbracket\n(u_max=10)\n",
  "random"="one\nrandom\nbracket\n"
)

plot_escore_1bracket = data.frame(p = seq(0.5, 1, length=101)) %>%
  rowwise() %>%
  mutate(
    random = emaxscore_random1(p),
    chalky3 = emaxscore_chalky1_umax(p, u_max=3),
    chalky10 = emaxscore_chalky1_umax(p, u_max=10)
  ) %>%
  pivot_longer(c(random, chalky3, chalky10)) %>%
  ggplot(aes(x=p)) +
  geom_line(aes(y=value, color=name), linewidth=1) +
  ylab("expected Hamming score") +
  scale_color_manual(
    name="", values=color_vec, labels=name_vec1
  )
plot_escore_1bracket
ggsave("plot_thm1/plot_escore_1bracket.png", plot_escore_1bracket, width=8, height=5)

###############################################
### Expected Max Score of n Random Brackets ###
###############################################

##### Poisson Binomial distribution from package `poibin`
##### make sure not to use the glitchy AF package `poisbinom`
##### https://www.sciencedirect.com/science/article/abs/pii/S0167947312003568

# source("a2_main.R"); m=63; p=.77

cdf_Mn_random_given_u <- function(p,n,u,k) {
  pb_probs = c(rep(p, m-u), rep(1-p, u)) # binomial probabilities
  poibin::ppoibin(k, pp=pb_probs)^n # poisson-binomial distribution 
}
### check
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=45)
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=47)
# cdf_Mn_random_given_u(p=p, n=10, u=10, k=50)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=45)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=47)
# cdf_Mn_random_given_u(p=p, n=100, u=10, k=50)

cdf_Mn_random <- function(p,n,k) {
  vec = numeric(m+1)
  for (u in 0:m) {
    vec[u+1] = cdf_Mn_random_given_u(p,n,u,k) * dbinom(u, size=m, prob=1-p)
  }
  sum(vec)
}
### check
# cdf_Mn_random(p=p, n=10, k=45)
# cdf_Mn_random(p=p, n=10, k=47)
# cdf_Mn_random(p=p, n=10, k=50)
# cdf_Mn_random(p=p, n=100, k=45)
# cdf_Mn_random(p=p, n=100, k=47)
# cdf_Mn_random(p=p, n=100, k=50)

escore_Mn_random <- function(p,n) {
  vec = numeric(m+1)
  for (k in 0:m) {
    vec[k+1] = 1 - cdf_Mn_random(p,n,k)
  }
  sum(vec)
}
### check
# escore_Mn_random(p=p, n=10)
# escore_Mn_random(p=p, n=100)
# escore_Mn_random(p=p, n=1000)
# escore_Mn_random(p=p, n=10000)
# escore_Mn_random(p=p, n=100000)
# escore_Mn_random(p=0.8, n=100)
# escore_Mn_random(p=0.9, n=100)
# escore_Mn_random(p=1, n=100)

###############################################
### Expected Max Score of n Chalky Brackets ###
###############################################

h <- function(y,m,u,v) {
  ### y is a vector of integers; return(sum(h(y)))
  ### m is an integer, the total number of bits
  ### u is an integer, number of bits that are 0 in tau
  ### v is an integer, number of bits that are 0 in x
  u1 = max(u,v)
  d = y - abs(u-v)
  d1 = d/2
  d1_L = 0
  d1_U = min(u,v,m-u1)
  
  d11 = d1[d1_L <= d1 & d1 <= d1_U & d %% 2 == 0]
  vec = choose(u1, abs(u-v)+d11) * choose(m-u1, d11)
  return(sum(vec))
}
# ### check
# h(y=0,m=12,u=8,v=5)
# h(y=1,m=12,u=8,v=5)
# h(y=2,m=12,u=8,v=5)
# h(y=3,m=12,u=8,v=5)
# choose(8,3)
# h(y=4,m=12,u=8,v=5)
# h(y=5,m=12,u=8,v=5)
# choose(8,4)*choose(4,1)
# h(y=6,m=12,u=8,v=5)
# h(y=7,m=12,u=8,v=5)
# choose(8,5)*choose(4,2)
# h(y=8,m=12,u=8,v=5)
# h(y=9,m=12,u=8,v=5)
# choose(8,6)*choose(4,3)
# h(y=10,m=12,u=8,v=5)
# h(y=11,m=12,u=8,v=5)
# choose(8,7)*choose(4,4)
# h(y=12,m=12,u=8,v=5)
# h(y=13,m=12,u=8,v=5)
# h(y=14,m=12,u=8,v=5)
# h(y=0:12,m=12,u=8,v=5)
# h(y=3,m=12,u=8,v=5)+h(y=5,m=12,u=8,v=5)+h(y=7,m=12,u=8,v=5)+h(y=9,m=12,u=8,v=5)+h(y=11,m=12,u=8,v=5)
  
cdf_Mn_chalky_given_u <- function(p,n,u,k,u_max=10) {
  vec = numeric(u_max+1)
  for (v in 0:u_max) {
    # browser()
    t1 = h(y = ((m-k):m) , m,u,v)
    t2 = h(y = 0:m , m,u,v)
    t3 = dbinom(v, size=m, prob=1-p)/pbinom(u_max, size=m, prob=1-p, lower.tail = TRUE)
    vec[v+1] = t1/t2 * t3
  }
  sum(vec)^n
}
### check
# cdf_Mn_chalky_given_u(p=p, n=5, u=10, k=50, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=5, u=10, k=51, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=50, u=10, k=50, u_max=4)
# cdf_Mn_chalky_given_u(p=p, n=50, u=10, k=51, u_max=4)

cdf_Mn_chalky <- function(p,n,k,u_max=10) {
  vec = numeric(m+1)
  for (u in 0:m) {
    vec[u+1] = cdf_Mn_chalky_given_u(p,n,u,k,u_max=u_max) * dbinom(u, size=m, prob=1-p)
  }
  sum(vec)
}
### check
# cdf_Mn_chalky(p=p, n=10, k=25)
# cdf_Mn_chalky(p=p, n=10, k=28)
# cdf_Mn_chalky(p=p, n=100, k=25)
# cdf_Mn_chalky(p=p, n=100, k=28)

escore_Mn_chalky <- function(p,n,u_max=10) {
  print(paste0("escore_Mn_chalky, ", "p=", p, ", n=", n, ", u_max=", u_max))
  vec = numeric(m+1)
  for (k in 0:m) {
    vec[k+1] = 1 - cdf_Mn_chalky(p,n,k,u_max=u_max)
  }
  sum(vec)
}
### check
# escore_Mn_chalky(p=0.5, n=10)
# escore_Mn_chalky(p=0.6, n=10)
# escore_Mn_chalky(p=0.7, n=10)
# escore_Mn_chalky(p=0.7, n=100)

#################################################
### Plot the Expected Max Score of n Brackets ###
#################################################

################# plot: check n=1 matches ################# 

# ### takes 2 mins
# plot_df_check_n_and_1 = tibble(p=seq(0.5, 0.99, length=101)) %>%
#   rowwise() %>%
#   mutate(
#     random_1 = emaxscore_random1(p),
#     chalky10_1 = emaxscore_chalky1_umax(p, u_max=10),
#     random_n1 = escore_Mn_random(p,n=1),
#     chalky10_n1 = escore_Mn_chalky(p,n=1,u_max=10),
#   )
# 
# plot_check_n_and_1 = 
#   plot_df_check_n_and_1 %>%
#   pivot_longer(c(random_1, chalky10_1, random_n1, chalky10_n1)) %>%
#   ggplot(aes(x=p)) +
#   geom_line(aes(y=value, color=name), linewidth=1) +
#   # scale_color_manual(name = "") +
#   ylab("expected Hamming score") 
# plot_check_n_and_1
# ggsave("plot_thm1/plot_check_n_and_1.png", 
#        plot_check_n_and_1, width=8, height=5)

##### tibbles containing expected score as a function of (n,p) for random brackets ##### 

plot_grid = expand.grid(p=seq(0.5, 0.99, length=101), n=10^(0:8)) %>% as_tibble()

# ### takes 2 minutes
# plot_df_escore_nRandomBrackets =
#   plot_grid %>%
#   rowwise() %>%
#   mutate(
#     random = escore_Mn_random(p,n)
#   )
# write_csv(plot_df_escore_nRandomBrackets, "plot_thm1/plot_df_escore_nRandomBrackets.csv")

plot_df_escore_nRandomBrackets = read_csv("plot_thm1/plot_df_escore_nRandomBrackets.csv")
plot_df_escore_nRandomBrackets

################# plot expected max score of n random brackets, varying n ################# 

my_palette_1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

plot_escore_nRandomBrackets = 
  plot_df_escore_nRandomBrackets %>%
  mutate(n = factor(n)) %>%
  # pivot_longer(c(random, chalky)) %>%
  ggplot(aes(x=p)) +
  geom_line(aes(y=random, color=n), linewidth=1) +
  scale_color_manual(values= my_palette_1) +
  labs(title="n random brackets") +
  ylab("expected max Hamming score") 
plot_escore_nRandomBrackets
ggsave("plot_thm1/plot_escore_nRandomBrackets.png", 
       plot_escore_nRandomBrackets, width=8, height=5)

##### tibbles containing expected score as a function of (n,p) for chalky brackets ##### 

# UMAX = 3 #FIXME #3 #10
for (UMAX in 1:25) {
  ### takes 5 minutes
  plot_df_escore_nChalkyBrackets =
    plot_grid %>%
    rowwise() %>%
    mutate(
      chalky = escore_Mn_chalky(p,n,u_max=UMAX),
    )
  write_csv(plot_df_escore_nChalkyBrackets, paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
}


# plot_df_escore_nChalkyBrackets = read_csv(paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
# plot_df_escore_nChalkyBrackets

plot_df_escore_nChalkyBrackets = tibble()
for (UMAX in 1:25) {
  plot_df_escore_nChalkyBrackets_umax = read_csv(paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
  plot_df_escore_nChalkyBrackets_umax$u_max = UMAX
  plot_df_escore_nChalkyBrackets = bind_rows(plot_df_escore_nChalkyBrackets, plot_df_escore_nChalkyBrackets_umax)
}

# plot_df_escore_nBrackets = 
#   left_join(plot_df_escore_nRandomBrackets, plot_df_escore_nChalkyBrackets) %>%
#   pivot_longer(c(random, chalky))

################# plot expected max score of n chalky brackets, varying n ################# 

# # UMAX = 10
# for (UMAX in 1:25) {
#   plot_escore_nChalkyBrackets = 
#     plot_df_escore_nChalkyBrackets %>%
#     filter(u_max == UMAX) %>%
#     mutate(n = factor(n)) %>%
#     ggplot(aes(x=p)) +
#     geom_line(aes(y=chalky, color=n), linewidth=1) +
#     scale_color_manual(values= my_palette_1) +
#     labs(title=paste0("n chalky brackets (u_max=",UMAX,")")) +
#     ylab("expected max Hamming score") 
#   plot_escore_nChalkyBrackets
#   ggsave(paste0("plot_thm1/plot_escore_nChalkyBrackets_", "u_max=",UMAX,".png"), 
#          plot_escore_nChalkyBrackets, width=8, height=5)
# }

plot_escore_nChalkyBrackets_vary_umax = 
  plot_df_escore_nChalkyBrackets %>%
  filter(u_max %% 3 == 1) %>%
  mutate(u_max_ = paste0("u_max = ", u_max)) %>%
  mutate(n = factor(n)) %>%
  ggplot(aes(x=p)) +
  facet_wrap(~fct_reorder(u_max_, u_max)) +
  geom_line(aes(y=chalky, color=n), linewidth=1) +
  scale_color_manual(values= my_palette_1) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title=paste0("n chalky brackets")) +
  ylab("expected max Hamming score") 
plot_escore_nChalkyBrackets_vary_umax
ggsave(paste0("plot_thm1/plot_escore_nChalkyBrackets_vary_umax.png"), 
       plot_escore_nChalkyBrackets_vary_umax, width=12, height=9)

################# plot expected max score of n chalky and random brackets ################# 

# color_vec_n1 =  c(
#   "chalky"="firebrick", 
#   "random"="dodgerblue2"
# )
my_palette_n1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:4]#[3:9]
)

# UMAX = 10
for (UMAX in seq(1,25,by=3)) {
  plot_escore_nbrackets_1 = 
    left_join(plot_df_escore_nChalkyBrackets,
              plot_df_escore_nRandomBrackets) %>%
    # filter(u_max %% 3 == 1) %>%
    filter(u_max==UMAX) %>%
    pivot_longer(c(random, chalky)) %>%
    mutate(
      name = ifelse(name=="chalky", paste0("chalky (u_max=",UMAX,")"), name),
      n_ = paste0("n=",n)
    ) %>%
    ggplot(aes(x=p)) +
    facet_wrap(~n_) + 
    geom_line(aes(y=value, color=name), linewidth=1) +
    scale_color_manual(values=c("firebrick", "dodgerblue2"), name="") +
    theme(panel.spacing = unit(2, "lines")) +
    ylab("expected max Hamming score") 
  plot_escore_nbrackets_1
  ggsave(paste0("plot_thm1/plot_escore_nbrackets_1_", "u_max=",UMAX,".png"), plot_escore_nbrackets_1, width=12, height=7)
}




my_palette_n2 = c(
  # rev(brewer.pal(name="Purples",n=9)[4:7]),
  # brewer.pal(name="Reds",n=9)[3:7]#[3:9]
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:6]#[3:9]
)

plot_escore_nbrackets_2 = 
  left_join(plot_df_escore_nChalkyBrackets,
            plot_df_escore_nRandomBrackets) %>%
  filter(u_max %% 3 == 1) %>%
  # filter(u_max==UMAX) %>%
  # mutate(u_max_ = paste0("u_max = ", u_max)) %>%
  pivot_longer(c(random, chalky)) %>%
  mutate(
    name = ifelse(name=="chalky", paste0("chalky (u_max=",u_max,")"), name),
    name_ = factor(name, levels=c( paste0("chalky (u_max=",seq(1,25,by=3),")"), "random") ),
    n_ = paste0("n=",n)
  ) %>%
  ggplot(aes(x=p)) +
  facet_wrap(~n_) + 
  geom_line( 
      aes(y=value, 
          color=name_
          # color=fct_reorder(name, u_max)
        ), 
      linewidth=1) +
  # geom_line(data = . %>% filter((name %in% "random")), linewidth=1.5, linetype="longdash", aes(y=value, color="random")) +
  # geom_line(data = . %>% filter(!(name %in% "random")), 
  #           aes(y=value, color=fct_reorder(name, u_max)), linewidth=1) +
  # scale_color_manual(values=c("firebrick", "random"="dodgerblue2"), name="") +
  scale_color_manual(values=c(my_palette_n2, "green"), name="") +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("expected max Hamming score") 
plot_escore_nbrackets_2
ggsave(paste0("plot_thm1/plot_escore_nbrackets_2.png"), plot_escore_nbrackets_2, width=12, height=7)
 
