
source("b8_simplified_MM_espn_main.R")

df_eMaxEspnScoreSmm = read_csv(paste0("df_eMaxEspnScoreSmm.csv"))
p_ = 0.75

df_eMaxEspnScoreSmm_1 = df_eMaxEspnScoreSmm %>%
  pivot_longer(starts_with("eMax")) %>%
  mutate(n = str_sub(name, start=17)) %>%
  mutate(n = as.numeric(n)) %>%
  rename(eMaxScore = value)
df_eMaxEspnScoreSmm_1

my_palette_npq_v1 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), #[3:9]
  "gray50",
  # rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:8]#[3:9]
)

#############
### Plots ###
#############

df_eMaxEspnScoreSmm_1 %>%
  ggplot(aes(x=q1, y=eMaxScore, color=factor(q6))) +
  facet_wrap(~n) +
  labs(title="p=0.75") +
  # guides(fill=guide_legend(title=TeX("$q_L$"))) +
  ylab("expected maximum ESPN score") +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_line(linewidth=1) +
  # xlab(TeX("$q_E$")) +
  # scale_color_manual(name=TeX("$q_L$"), values=my_palette_npq_v1)
  xlab(TeX("$q_1=q_2=q_3$")) +
  scale_color_manual(name=TeX("$q_4=q_5=q_6$"), values=my_palette_npq_v1)

df_eMaxEspnScoreSmm_1 %>%
  filter(q1 == q6) %>% ### just one q
  mutate(n = factor(n)) %>%
  ggplot(aes(x=q1, y=eMaxScore, color=n)) +
  labs(title="p=0.75") +
  xlab("q") + 
  ylab("expected maximum ESPN score") +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_line(linewidth=1) +
  scale_color_manual(values=my_palette_npq_v1)


df_eMaxEspnScoreSmm_1 %>%
  ggplot(aes(x=q1, y=q6, fill=eMaxScore)) +
  facet_wrap(~n) +
  geom_tile() +
  labs(title="p=0.75") +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
  scale_fill_gradientn(name="Expected\nMax\nScore", colours = rev(terrain.colors(7))) +
  xlab(TeX("$q_1=q_2=q_3$")) + ylab(TeX("$q_4=q_5=q_6$"))



