
source("b5_simplified_MM_main.R")

plot_grid_npq = expand.grid(
  # p = seq(0.5, 0.99, length=50),
  # q = seq(0.5, 0.99, length=50),
  # p = seq(0.5, 1, by=0.05),
  # q = seq(0.05, 1, by=0.05),
  p = seq(0.5, 1, by=0.1),
  q = seq(0, 1, by=0.1),
  n = 10^(0:8)
)
# as_tibble(plot_grid_npq)

##### tibble of expected score as a function of (n,p,q) for q-chalky brackets ##### 
filename = "plot_thm1/df_eMaxScore_q_chalky_npq.csv"
if ( !file.exists((filename)) ) { 
  result = numeric(nrow(plot_grid_npq))
  for (i in 1:nrow(plot_grid_npq)) {
    print(paste0("i = ", i, " of ", nrow(plot_grid_npq)))
    print(plot_grid_npq[i,])
    
    n = plot_grid_npq[i,]$n
    p = plot_grid_npq[i,]$p
    q = plot_grid_npq[i,]$q
    
    result[i] = eMaxScore_q_chalky(m=m,n=n,p=p,q=q)
  }
  plot_grid_npq$eMaxScore = result
  write_csv(plot_grid_npq, paste0(filename))
} else {
  plot_grid_npq = read_csv(filename)
}

#############
### PLOTS ###
#############

# my_palette_npq = c(
#   rev(brewer.pal(name="Purples",n=9)[3:9]),
#   rev(brewer.pal(name="Reds",n=9)[3:9]), #[3:9]
#   # rev(brewer.pal(name="PuRd",n=9)[4:9]),
#   brewer.pal(name="Blues",n=9)[3:9]#[3:9]
# )

#### plot 1 ####

my_palette_npq = c(
  rev(brewer.pal(name="Reds",n=9)[4:8]), #[3:9]
  "gray50",
  # rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:8]#[3:9]
)

plot_eMaxScore_npq = plot_grid_npq %>%
  # filter(0.2 <= q) %>%
  mutate(
    n_ = paste0("n = ", n),
    q = factor(q)
  ) %>%
  ggplot(aes(x=p,color=q,y=eMaxScore)) +
  facet_wrap(~n_) +
  geom_line(linewidth=1) +
  geom_line(data = . %>% filter(p==q), color="green", linewidth=1.5) +
  scale_color_manual(name="q", values=my_palette_npq) +
  theme(panel.spacing = unit(2, "lines")) +
  # geom_point() +
  ylab("expected max Hamming score")
# plot_eMaxScore_npq
ggsave("plot_thm1/plot_npq_eMaxScore.png", plot_eMaxScore_npq, width=12,height=8)

#### plot 2 ####

my_palette_npq1 = c(
  # rev(brewer.pal(name="Reds",n=9)[1:9]), #[3:9]
  # "gray50",
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:9]#[3:9]
)

df_q_star_npq = plot_grid_npq %>%
  group_by(n,p) %>%
  summarise(
    opt = eMaxScore == max(eMaxScore),
    count = sum(opt),
    q_star = q[opt]
  )
df_q_star_npq
sum(df_q_star_npq$count!=1)

plot_q_star_npq = 
  df_q_star_npq %>%
  filter(p > 0.5 & p < 1) %>%
  ggplot(aes(x=p,y=q_star,color=factor(n))) +
  # geom_line(data = . %>% filter(p==q_star), color="gray50", linetype="dashed", linewidth=1, alpha=0.4) +
  geom_abline(intercept=0, slope=1, color="gray50", linetype="dashed", linewidth=1, alpha=0.4) +
  geom_point(size=3) +
  ylab(TeX("$q^*$")) +
  scale_color_manual(name="n", values=my_palette_npq1)
plot_q_star_npq
ggsave("plot_thm1/plot_npq_q_star.png", plot_q_star_npq, width=8,height=5)


