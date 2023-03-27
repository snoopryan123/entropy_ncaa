
source("b7_entropy_range_WP_search_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

GRID_results = read_csv("df_entropy_range_grid_results.csv")
GRID_results = GRID_results %>% filter(opp_prob_method != "P_538_2022")

my_palette_0 = c(
  rev(brewer.pal(name="PuRd",n=9)[3:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

my_palette_a = c(
  "firebrick", "orange", "dodgerblue"
)

GRID_results %>%
  pivot_longer(c("hU_star_espn", "hU_star_num_correct")) %>%
  rename(hU_star = value) %>%
  rename(scoring_method = name) %>%
  mutate(scoring_method = str_sub(scoring_method, start=9)) %>%
  mutate(opp_prob_method_ = case_when(
          opp_prob_method=="naive_chalky" ~ "colloquially chalky opponents",
          opp_prob_method=="P_538_2022" ~ "purely random opponents",
          opp_prob_method=="naive_random"~"\ncolloquially\nrandom\nopponents\n"
        ),
        # opp_prob_method_ = paste0("opponents: ", opp_prob_method_)
  ) %>%
  mutate(
    scoring_method_ = case_when(
      scoring_method=="espn"~"ESPN score",
      scoring_method=="num_correct"~"Hamming score"
    )
  ) %>%
  ggplot() +
  # facet_wrap(~ opp_prob_method_ + scoring_method_) +
  facet_wrap(~ scoring_method_ + opp_prob_method_) +
  # scale_color_manual(values=my_palette_a) +
  scale_color_manual(name="n", values=my_palette_a) +
  theme(panel.spacing = unit(3, "lines")) +
  geom_point(aes(y=hU_star, x=log(k,base=10), color=factor(n)), size=5) +
  scale_x_continuous(breaks=0:10, labels=10^(0:10)) +
  # xlab(TeX("$log_{10}(k)$")) +
  xlab("k") +
  ylab(TeX("$h_U^*$")) 
  

GRID_results %>%
  pivot_longer(c("hU_star_espn", "hU_star_num_correct")) %>%
  rename(hU_star = value) %>%
  rename(scoring_method = name) %>%
  mutate(scoring_method = str_sub(scoring_method, start=9)) %>%
  mutate(opp_prob_method_ = case_when(
    opp_prob_method=="naive_chalky" ~ "\ncolloquially\nchalky\nopponents\n",
    opp_prob_method=="P_538_2022" ~ "\npurely\nrandom\nopponents\n",
    opp_prob_method=="naive_random"~"\ncolloquially\nrandom\nopponents\n"
  ),
  # opp_prob_method_ = paste0("opponents: ", opp_prob_method_)
  ) %>%
  mutate(
    scoring_method_ = case_when(
      scoring_method=="espn"~"ESPN score",
      scoring_method=="num_correct"~"Hamming score"
    )
  ) %>%
  mutate(k_ = paste0("k = ", k)) %>%
  ggplot() +
  # facet_wrap(~ opp_prob_method_ + scoring_method_) +
  facet_wrap(~ scoring_method_ + k_) +
  # scale_color_manual(values=my_palette_a) +
  scale_color_manual(name="n", values=my_palette_a) +
  theme(panel.spacing = unit(3, "lines")) +
  geom_point(aes(x=hU_star, y=opp_prob_method_, color=factor(n)), size=5) +
  # scale_x_continuous(breaks=0:10, labels=10^(0:10)) +
  # # xlab(TeX("$log_{10}(k)$")) +
  # xlab("k") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("") + 
  xlab(TeX("$h_U^*$")) 


