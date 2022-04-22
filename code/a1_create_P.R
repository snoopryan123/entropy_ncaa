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
  rownames(P) = paste("Team", 1:n) #d538$team_name
  colnames(P) = paste("Team", 1:n) #d538$team_name
  
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

make_elo_plot <- function(elos) {
  elo_plot = tibble(elos = elos) %>% ggplot() + geom_histogram(aes(x=elos), bins=64, fill="black") +
    # labs(title="Histogram of Elo Ratings") +
    scale_x_continuous(name="Elo", breaks=seq(70,93,by=5)) 
  elo_plot
}

make_wp_plot <- function(P_matrix) {
  P1_tib = tibble(reshape2::melt(P_matrix) %>% 
                    rename(t1=Var1,t2=Var2,p=value) %>% 
                    arrange(t1,t2) %>% drop_na()) %>%
    mutate(t2_num = as.numeric(str_sub(t2,6)))
  P1_tib
  P1_plot = P1_tib %>% ggplot() +
    theme(legend.position="none") +
    geom_point(aes(x=t2_num, y=p, color=t1)) +
    scale_x_continuous(breaks=seq(0,64,by=10),name="team index j") +
    ylab("win probability")
  P1_plot
}




P_538_2022 = get_prob_matrix(d538$elo)
P_538_2022[1:6,1:6]
P_538_2022_elo_plot = make_elo_plot(d538$elo)
P_538_2022_elo_plot
P_538_2022_P_plot = make_wp_plot(P_538_2022)
P_538_2022_P_plot


write.csv(P_538_2022, "../data/P_538_2022.csv")
ggsave("plot_P_538_2022/P_538_2022_elo_plot.png", P_538_2022_elo_plot, width=6,height=6)
ggsave("plot_P_538_2022/P_538_2022_P_plot.png", P_538_2022_P_plot, width=6,height=6)


#################
### ELO Model ###
#################

d538 %>% ggplot() + geom_histogram(aes(x=elo), bins=20)
d538$elo

set.seed(77)
r_elos = rnorm(100, mean(d538$elo), sd(d538$elo))
r_elos = r_elos[70 <= r_elos & r_elos <= 93][1:64]
r_elos = sort(r_elos,decreasing = TRUE)


elo_plot = make_elo_plot(r_elos)
elo_plot

P1 = get_prob_matrix(r_elos)


P1_plot = make_wp_plot(P1)
P1_plot


P1[1:6,1:6]

ggsave("plot_P1/P1_elo_plot.png", elo_plot, width=7, height=6)
ggsave("plot_P1/P1_win_probs.png", P1_plot, width=6,height=6)

write.csv(P1, "../data/P1.csv")

