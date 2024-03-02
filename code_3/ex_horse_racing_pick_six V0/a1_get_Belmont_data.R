
####################################
### Kentucky Derby 2023 Pick Six ###
####################################

source("../a0_loadStuff.R")
output_folder = "plots/"

###########################################################
### Acquire Belmont Park racing data from June 10, 2023 ###
###########################################################

library(rvest)

### Churchill Downs May 6, 2023 (Kentucky Derby 2023)
# link "https://entries.horseracingnation.com/entries-results/churchill-downs/2023-05-06"

###  Belmont Park June 10, 2023 (Belmont Stakes 2023)
# link_ = "https://entries.horseracingnation.com/entries-results/belmont-park/2023-06-10"

###  Belmont Park May 21, 2023 ($38k pick 6 carryover)
# https://www.nyra.com/belmont/news/pick-6-carryover-of-$38k-on-sunday-at-belmont-park
link_ = "https://entries.horseracingnation.com/entries-results/belmont-park/2023-05-21"

###
html_ = read_html(link_)
TBLS = html_table(html_)
as.numeric(lapply(TBLS, ncol))

TBLS

RACES = TBLS[seq(1, length(TBLS), by=3)]
RESULTS = TBLS[seq(2, length(TBLS), by=3)]

RACES[[1]]
RESULTS[[1]]

df_transform_races = function(df) {
  # RACES[[1]] %>%
  df %>%
    select(X2,X4,X5,X7) %>%
    mutate(X7 = str_remove_all(X7, "\n")) %>%
    mutate(X7 = str_remove_all(X7, " ")) %>%
    mutate(X7 = str_remove_all(X7, "AE")) %>%
    mutate(X7 = str_remove_all(X7, "MTO")) %>%
    rename(PP = X2) %>%
    mutate(horse = sapply(str_split(X4,"\n"), function(x) x[[1]])) %>%
    mutate(sire  = sapply(str_split(X4,"\n"), function(x) x[[2]])) %>%
    mutate(sire = str_remove(sire, "^        ")) %>%
    mutate(trainer = sapply(str_split(X5,"\n"), function(x) x[[1]])) %>%
    mutate(jockey = sapply(str_split(X5,"\n"), function(x) x[[2]])) %>%
    mutate(jockey = str_remove(jockey, "^        ")) %>%
    mutate(ML_top = as.numeric(sapply(str_split(X7,"/"), function(x) x[[1]]))) %>%
    mutate(ML_bot = as.numeric(sapply(str_split(X7,"/"), function(x) x[[2]]))) %>% 
    select(-c(X4,X5,X7)) %>%
    mutate(ML_decimal_odds = ML_top/ML_bot+1) %>% 
    select(-c(ML_bot, ML_top)) %>%
    mutate(probs_raw = 1/ML_decimal_odds*100) %>%
    mutate(probs = probs_raw/sum(probs_raw)) %>% select(-probs_raw)
}

df_transform_results = function(df) {
  # RESULTS[[1]][,1] %>%
  df[,1] %>%
    mutate(result = row_number()) %>%
    mutate(winner = as.numeric(result == 1)) %>%
    rename(horse = Runner)
}

# left_join(df_transform_races(RACES[[1]]), df_transform_results(RESULTS[[1]])) %>%
#   mutate(race_num = 1)

RACES1 = lapply(RACES, df_transform_races)
RESULTS1 = lapply(RESULTS, df_transform_results)

# RACES[[5]]
# RACES1[[5]]

RACES1
RESULTS1

RACES2 = lapply(
  1:length(RACES1),
  function(i) {
    left_join(df_transform_races(RACES[[i]]), df_transform_results(RESULTS[[i]])) %>%
      mutate(race_num = i,
             winner = ifelse(is.na(winner), 0, winner)) 
  }
)
RACES2

RACES3 = do.call("rbind", RACES2)
RACES3

### save data
# write_csv(RACES3, "df_Belmont_6-10-23.csv")
write_csv(RACES3, "df_Belmont_5-21-23.csv")

