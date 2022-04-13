library(tidyverse)
# library(purrr)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))

SAVE_PLOT = TRUE #TRUE #FALSE

D = read_csv("../data/538_ELO.csv") %>% rename(game_id = initial_game_num) ##%>% arrange(game_id)

P_file = "P1" # "P_538_2022"
plot_folder = paste0("plot_", P_file, "/")
P_ = read.csv( paste0("../data/", P_file, ".csv"), row.names = 1, header= TRUE)

P <- function(x) {
  # x = c(i,j)    ### return the probability team idx i beats team idx j
  i = x[1]; j = x[2];
  P_[i,j]
}
