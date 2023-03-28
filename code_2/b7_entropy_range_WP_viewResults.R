
source("b7_entropy_range_WP_search_main.R")

### compute mean entropy
bracket_set_colloquially_chalky = sample_n_brackets(1e5, prob_method="naive_chalky", keep_probs=T)
bracket_set_colloquially_random = sample_n_brackets(1e5, prob_method="naive_random", keep_probs=T)
bracket_set_pure_random = sample_n_brackets(1e5, prob_method="P_538_2022", keep_probs=T)
bracket_set_super_chalky = sample_n_brackets_entropyRange(1e5, hL=-Inf, hU=45.75)

mean(compute_entropies(bracket_set_colloquially_chalky))
mean(compute_entropies(bracket_set_colloquially_random))
mean(compute_entropies(bracket_set_pure_random))
mean(compute_entropies(bracket_set_super_chalky))

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

GRID_results = read_csv("df_entropy_range_grid_results.csv")
# GRID_results = read_csv("df_entropy_range_grid_results OLD.csv")

GRID_results =
  GRID_results %>%
  filter(opp_prob_method != "naive_chalky")
  # filter(opp_prob_method != "naive_random")
  # filter(opp_prob_method != "P_538_2022")

GRID_results

df_wp_entropy_range_results = 
  GRID_results %>%
  pivot_longer(c("hU_star_espn", "hU_star_num_correct")) %>%
  rename(hU_star = value) 
df_wp_entropy_range_results 
mean_entropies = 
  sapply(
    1:nrow(df_wp_entropy_range_results),
    function(i) mean(compute_entropies(sample_n_brackets_entropyRange(250, -Inf, df_wp_entropy_range_results$hU_star[i])))
)
df_wp_entropy_range_results$mean_entropies = mean_entropies
df_wp_entropy_range_results

my_palette_0 = c(
  rev(brewer.pal(name="PuRd",n=9)[3:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

my_palette_a = c(
  "firebrick", "orange", "dodgerblue"
)


plot_wp_entropy_range_results = 
  df_wp_entropy_range_results %>%
  rename(scoring_method = name) %>%
  mutate(scoring_method = str_sub(scoring_method, start=9)) %>%
  mutate(
    opp_entropy = case_when(
      opp_prob_method=="naive_chalky" ~ 48.2,
      opp_prob_method=="P_538_2022" ~ 48.7,
      opp_prob_method=="naive_random"~52.3,
      opp_prob_method=="super_chalky"~44
    ),
  ) %>%
  mutate(opp_prob_method_ = case_when(
    opp_prob_method=="naive_chalky" ~ "\ncolloquially\nchalky\nopponents\n",
    opp_prob_method=="P_538_2022" ~ "\npurely\nrandom\nopponents\n",
    opp_prob_method=="naive_random"~"\ncolloquially\nrandom\nopponents\n",
    opp_prob_method=="super_chalky"~"\nquite\nchalky\nopponents\n"
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
  theme(panel.spacing = unit(3, "lines")) +
  geom_point(aes(
    # x=hU_star, 
    x = mean_entropies,
    # stroke=(n==k)*2 ,
    y=opp_prob_method_, 
    fill=factor(n)
    ), shape=21, color="black", size=5) +
  # xlab(TeX("$h_U^*$")) +
  xlab("mean entropy of the submitted bracket set") +
  scale_fill_manual(name="n", values=my_palette_a) +
  geom_point(aes(x=opp_entropy, y=opp_prob_method_), color="gray20", shape=4, size=3) +
  geom_vline(xintercept=48.7, color="gray60", linetype="dashed") +
  # geom_vline(xintercept=63, color="gray60", linetype="dashed") +
  # scale_x_continuous(breaks=0:10, labels=10^(0:10)) +
  # # xlab(TeX("$log_{10}(k)$")) +
  # xlab("k") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(size=14)) +
  ylab("") 
  # ylab("opponents' bracket strategy") 
# plot_wp_entropy_range_results
ggsave("plot_wp_entropy_range_results.png",
       plot_wp_entropy_range_results,
       width=14, height=8)






# GRID_results %>%
#   pivot_longer(c("hU_star_espn", "hU_star_num_correct")) %>%
#   rename(hU_star = value) %>%
#   rename(scoring_method = name) %>%
#   mutate(scoring_method = str_sub(scoring_method, start=9)) %>%
#   mutate(opp_prob_method_ = case_when(
#           opp_prob_method=="naive_chalky" ~ "colloquially chalky opponents",
#           opp_prob_method=="P_538_2022" ~ "purely random opponents",
#           opp_prob_method=="naive_random"~"\ncolloquially\nrandom\nopponents\n"
#         ),
#         # opp_prob_method_ = paste0("opponents: ", opp_prob_method_)
#   ) %>%
#   mutate(
#     scoring_method_ = case_when(
#       scoring_method=="espn"~"ESPN score",
#       scoring_method=="num_correct"~"Hamming score"
#     )
#   ) %>%
#   ggplot() +
#   # facet_wrap(~ opp_prob_method_ + scoring_method_) +
#   facet_wrap(~ scoring_method_ + opp_prob_method_) +
#   # scale_color_manual(values=my_palette_a) +
#   scale_color_manual(name="n", values=my_palette_a) +
#   theme(panel.spacing = unit(3, "lines")) +
#   geom_point(aes(y=hU_star, x=log(k,base=10), color=factor(n)), size=5) +
#   scale_x_continuous(breaks=0:10, labels=10^(0:10)) +
#   # xlab(TeX("$log_{10}(k)$")) +
#   xlab("k") +
#   ylab(TeX("$h_U^*$")) 

