
source("a2_main.R")

######################################################################
### To maximize your probability of winning the bracket challenge, ###
### given the "true" known win probabilities P,                    ###
### given that the opponent submits k naively chalky brackets,     ###
### and given that you submit n brackets, from what entropy range  ###
### should you submit brackets?                                    ###
######################################################################

n_ = 2500
# GRID_h = 38:58
GRID_h = 40:58
entropies_left_tail = numeric(length(GRID_h))
entropies_right_tail = numeric(length(GRID_h))
for (i in 1:length(GRID_h)) {
    print(c(i))
    h = GRID_h[i]
    bracket_set_left_tail = sample_n_brackets_entropyRange(n_, -Inf, h)
    bracket_set_right_tail = sample_n_brackets_entropyRange(n_, h, Inf)
    entropies_left_tail[i] = mean(compute_entropies(bracket_set_left_tail))
    entropies_right_tail[i] = mean(compute_entropies(bracket_set_right_tail))
}

df_entropy_conversion = tibble(
  h = c(GRID_h, GRID_h),
  left_tail = c(rep(T, length(GRID_h)), rep(F, length(GRID_h))),
  H = c(entropies_left_tail, entropies_right_tail),
) 
write_csv(df_entropy_conversion, "df_entropy_conversion.csv")

plot_entropy_conversion = 
  tibble(
    h = c(paste0("(-", bquote("\U221E"),", ", GRID_h, "]"), paste0("[", GRID_h, ", ",bquote("\U221E"),"]")),
    H = c(entropies_left_tail, entropies_right_tail),
  ) %>%
  filter(row_number() %% 2 == 1) %>%
  ggplot() +
  geom_point(aes(x=h, y=H), size=3) +
  xlab("entropy range of sampled brackets") +
  ylab("mean entropy of sampled brackets") +
  geom_hline(yintercept=48.7, color="gray60", linetype="dashed") +
  theme(
    # axis.title.x = element_text(size=15),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15)
  )
# plot_entropy_conversion
ggsave("plot_entropy_conversion.png", plot_entropy_conversion,
       width=10, height=7)

