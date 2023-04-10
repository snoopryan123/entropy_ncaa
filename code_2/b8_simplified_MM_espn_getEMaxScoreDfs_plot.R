
source("b8_simplified_MM_espn_main.R")

df_eMaxEspnScoreSmm_v1 = read_csv(paste0("df_eMaxEspnScoreSmm_v1.csv"))
p_ = 0.75

df_eMaxEspnScoreSmm_v1a = df_eMaxEspnScoreSmm_v1 %>%
  mutate(q = q1) %>%
  select(-c(q1,q2,q3,q4,q5,q6)) %>%
  pivot_longer(starts_with("eMax")) %>%
  mutate(n = str_sub(name, start=17)) %>%
  mutate(n = as.numeric(n)) %>%
  rename(eMaxScore = value)
df_eMaxEspnScoreSmm_v1a

# df_eMaxEspnScoreSmm_v2a = df_eMaxEspnScoreSmm_v2 %>%
#   mutate(qE = q1, qL = q6) %>%
#   select(-c(q1,q2,q3,q4,q5,q6)) %>%
#   pivot_longer(starts_with("eMax")) %>%
#   mutate(n = str_sub(name, start=17)) %>%
#   mutate(n = as.numeric(n)) %>%
#   rename(eMaxScore = value)
# df_eMaxEspnScoreSmm_v2a

my_palette_npq_v1 = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), #[3:9]
  "gray50",
  # rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:8]#[3:9]
)

###########################
### Plots: q1=q2=...=q6 ###
###########################

df_eMaxEspnScoreSmm_v1a %>%
  filter(score=="Hamming") %>%
  mutate(
    n_ = paste0("n = ", n),
    q = factor(q),
    # eMaxScore = m - eMaxScore/10
    eMaxScore = eMaxScore/10
  ) %>%
  ggplot(aes(x=p,color=q,y=eMaxScore)) +
  facet_wrap(~n_) +
  geom_line(linewidth=1) +
  geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
  scale_color_manual(name="q", values=my_palette_npq_v1) +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("expected max Hamming score")


df_eMaxEspnScoreSmm_v1a %>%
  filter(score=="ESPN") %>%
  mutate(
    n_ = paste0("n = ", n),
    q = factor(q),
  ) %>%
  ggplot(aes(x=p,color=q,y=eMaxScore)) +
  facet_wrap(~n_) +
  geom_line(linewidth=1) +
  geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
  scale_color_manual(name="q", values=my_palette_npq_v1) +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("expected max ESPN score")








#############
### Plots ###
#############

df_eMaxEspnScoreSmm_v1a %>%
  filter(score=="Hamming") %>%
  ggplot(aes(x=p, y=eMaxScore, color=factor(q))) +
  facet_wrap(~n) +
  # guides(fill=guide_legend(title=TeX("$q_L$"))) +
  ylab("expected maximum ESPN score") +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_line(linewidth=1) +
  scale_color_manual(values=my_palette_npq_v1)



# df_eMaxEspnScoreSmm_v1a %>%
  # filter(q1 == q6) %>% ### just one q
  # mutate(n = factor(n)) %>%
  # ggplot(aes(x=q1, y=eMaxScore, color=n)) +
  # facet_wrap(~score) +
  # labs(title="p=0.75") +
  # xlab("q") + 
  # ylab("expected maximum ESPN score") +
  # geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  # geom_line(linewidth=1) +
  # scale_color_manual(values=my_palette_npq_v1)



df_eMaxEspnScoreSmm_v1a %>%
  ggplot(aes(x=q1, y=q6, fill=eMaxScore)) +
  facet_wrap(~n) +
  geom_tile() +
  labs(title="p=0.75") +
  geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
  scale_fill_gradientn(name="Expected\nMax\nScore", colours = rev(terrain.colors(7))) +
  xlab(TeX("$q_1=q_2=q_3$")) + ylab(TeX("$q_4=q_5=q_6$"))



