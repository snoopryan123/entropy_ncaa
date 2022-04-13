library(tidyverse)

#############################
### Actual 2022 538 ELO's ###
#############################

d538 = read_csv("../data/538_ELO.csv")
n = 64 #nrow(d538)

elo_prob <- function(elo1, elo2) {
  1 / (1 + 10^(-(elo1 - elo2)*30.464/400)) 
} 

get_prob_matrix <- function(elos) {
  P = matrix(nrow=n, ncol=n)
  rownames(P) = d538$team_name
  colnames(P) = d538$team_name
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        elo_i = elos[i]
        elo_j = elos[j]
        P[i,j] = elo_prob(elo_i, elo_j)
      }
    }
  }
  
  P
}

P_538_2022 = get_prob_matrix(d538$elo)

P_538_2022[1:6,1:6]

write.csv(P_538_2022, "../data/P_538_2022.csv")

#################
### ELO Model ###
#################

d538 %>% ggplot() + geom_histogram(aes(x=elo), bins=20)
d538$elo


set.seed(77)
r_elos = rnorm(100, mean(d538$elo), sd(d538$elo))
r_elos = r_elos[70 <= r_elos & r_elos <= 93][1:64]
r_elos = sort(r_elos,decreasing = TRUE)
tibble(r_elos) %>% ggplot() + geom_histogram(aes(x=r_elos), bins=20)

P1 = get_prob_matrix(r_elos)

P1[1:6,1:6]

write.csv(P1, "../data/P1.csv")

