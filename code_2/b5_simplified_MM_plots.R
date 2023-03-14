
source("b5_simplified_MM_main.R")

###

##############################################
### Plot the Expected Score of One Bracket ###
##############################################

plot_escore_1bracket = data.frame(p = seq(0.5, 1, length=101)) %>%
  rowwise() %>%
  mutate(
    random = emaxscore_random1(p),
    chalky3 = emaxscore_chalky1_umax(p, u_max=3),
    chalky10 = emaxscore_chalky1_umax(p, u_max=10)
  ) %>%
  pivot_longer(c(random, chalky3, chalky10)) %>%
  ggplot(aes(x=p)) +
  geom_line(aes(y=value, color=name), linewidth=1) +
  ylab("expected Hamming score") +
  scale_color_manual(
    name="", values=color_vec, labels=name_vec1
  )
plot_escore_1bracket
ggsave("plot_thm1/plot_escore_1bracket.png", plot_escore_1bracket, width=8, height=5)

#################################################
### Plot the Expected Max Score of n Brackets ###
#################################################

################# plot: check n=1 matches ################# 

# ### takes 2 mins
# plot_df_check_n_and_1 = tibble(p=seq(0.5, 0.99, length=101)) %>%
#   rowwise() %>%
#   mutate(
#     random_1 = emaxscore_random1(p),
#     chalky10_1 = emaxscore_chalky1_umax(p, u_max=10),
#     random_n1 = escore_Mn_random(p,n=1),
#     chalky10_n1 = escore_Mn_chalky(p,n=1,u_max=10),
#   )
# 
# plot_check_n_and_1 = 
#   plot_df_check_n_and_1 %>%
#   pivot_longer(c(random_1, chalky10_1, random_n1, chalky10_n1)) %>%
#   ggplot(aes(x=p)) +
#   geom_line(aes(y=value, color=name), linewidth=1) +
#   # scale_color_manual(name = "") +
#   ylab("expected Hamming score") 
# plot_check_n_and_1
# ggsave("plot_thm1/plot_check_n_and_1.png", 
#        plot_check_n_and_1, width=8, height=5)

##### tibbles containing expected score as a function of (n,p) for random brackets ##### 

# ### takes 2 minutes
# plot_df_escore_nRandomBrackets =
#   plot_grid %>%
#   rowwise() %>%
#   mutate(
#     random = escore_Mn_random(p,n)
#   )
# write_csv(plot_df_escore_nRandomBrackets, "plot_thm1/plot_df_escore_nRandomBrackets.csv")

plot_df_escore_nRandomBrackets = read_csv("plot_thm1/plot_df_escore_nRandomBrackets.csv")
plot_df_escore_nRandomBrackets

################# plot expected max score of n random brackets, varying n ################# 

plot_escore_nRandomBrackets = 
  plot_df_escore_nRandomBrackets %>%
  mutate(n = factor(n)) %>%
  # pivot_longer(c(random, chalky)) %>%
  ggplot(aes(x=p)) +
  geom_line(aes(y=random, color=n), linewidth=1) +
  scale_color_manual(values= my_palette_1) +
  labs(title="n purely random brackets") +
  ylab("expected max Hamming score") 
plot_escore_nRandomBrackets
ggsave("plot_thm1/plot_escore_nRandomBrackets.png", 
       plot_escore_nRandomBrackets, width=8, height=5)

# plot_df_escore_nChalkyBrackets = read_csv(paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
# plot_df_escore_nChalkyBrackets

plot_df_escore_nChalkyBrackets = tibble()
for (UMAX in 1:25) {
  plot_df_escore_nChalkyBrackets_umax = read_csv(paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
  plot_df_escore_nChalkyBrackets_umax$u_max = UMAX
  plot_df_escore_nChalkyBrackets = bind_rows(plot_df_escore_nChalkyBrackets, plot_df_escore_nChalkyBrackets_umax)
}

# plot_df_escore_nBrackets = 
#   left_join(plot_df_escore_nRandomBrackets, plot_df_escore_nChalkyBrackets) %>%
#   pivot_longer(c(random, chalky))

################# plot expected max score of n chalky brackets, varying n ################# 

# # UMAX = 10
# for (UMAX in 1:25) {
#   plot_escore_nChalkyBrackets = 
#     plot_df_escore_nChalkyBrackets %>%
#     filter(u_max == UMAX) %>%
#     mutate(n = factor(n)) %>%
#     ggplot(aes(x=p)) +
#     geom_line(aes(y=chalky, color=n), linewidth=1) +
#     scale_color_manual(values= my_palette_1) +
#     labs(title=paste0("n chalky brackets (u_max=",UMAX,")")) +
#     ylab("expected max Hamming score") 
#   plot_escore_nChalkyBrackets
#   ggsave(paste0("plot_thm1/plot_escore_nChalkyBrackets_", "u_max=",UMAX,".png"), 
#          plot_escore_nChalkyBrackets, width=8, height=5)
# }

plot_escore_nChalkyBrackets_vary_umax = 
  plot_df_escore_nChalkyBrackets %>%
  filter(u_max %% 3 == 1) %>%
  # mutate(u_max_ = paste0("u_max = ", u_max)) %>%
  mutate(u_max_ = paste0("U = ", u_max)) %>%
  mutate(n = factor(n)) %>%
  ggplot(aes(x=p)) +
  facet_wrap(~fct_reorder(u_max_, u_max)) +
  geom_line(aes(y=chalky, color=n), linewidth=1) +
  scale_color_manual(values= my_palette_1) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title=paste0("n randomly sampled U-chalky brackets")) +
  ylab("expected max Hamming score") 
plot_escore_nChalkyBrackets_vary_umax
ggsave(paste0("plot_thm1/plot_escore_nChalkyBrackets_vary_umax.png"), 
       plot_escore_nChalkyBrackets_vary_umax, width=12, height=9)

################# plot expected max score of n chalky and random brackets ################# 

# UMAX = 10
for (UMAX in seq(1,25,by=3)) {
  plot_escore_nbrackets_1 = 
    left_join(plot_df_escore_nChalkyBrackets,
              plot_df_escore_nRandomBrackets) %>%
    # filter(u_max %% 3 == 1) %>%
    filter(u_max==UMAX) %>%
    pivot_longer(c(random, chalky)) %>%
    mutate(
      # name = ifelse(name=="chalky", paste0("chalky (u_max=",UMAX,")"), name),
      name = ifelse(name=="chalky", paste0("chalky (U=",UMAX,")"), name),
      n_ = paste0("n=",n)
    ) %>%
    ggplot(aes(x=p)) +
    facet_wrap(~n_) + 
    geom_line(aes(y=value, color=name), linewidth=1) +
    scale_color_manual(values=c("firebrick", "dodgerblue2"), name="") +
    theme(panel.spacing = unit(2, "lines")) +
    ylab("expected max Hamming score") 
  plot_escore_nbrackets_1
  ggsave(paste0("plot_thm1/plot_escore_nbrackets_1_", "u_max=",UMAX,".png"), plot_escore_nbrackets_1, width=12, height=7)
}





plot_escore_nbrackets_2 = 
  left_join(plot_df_escore_nChalkyBrackets,
            plot_df_escore_nRandomBrackets) %>%
  filter(u_max %% 3 == 1) %>%
  # filter(u_max==UMAX) %>%
  # mutate(u_max_ = paste0("u_max = ", u_max)) %>%
  rename(`purely random` = random) %>%
  pivot_longer(c(`purely random`, chalky)) %>%
  mutate(
    # name = ifelse(name=="chalky", paste0("chalky (u_max=",u_max,")"), name),
    # name_ = factor(name, levels=c( paste0("chalky (u_max=",seq(1,25,by=3),")"), "random") ),
    name = ifelse(name=="chalky", paste0("chalky (U=",u_max,")"), name),
    name_ = factor(name, levels=c( paste0("chalky (U=",seq(1,25,by=3),")"), "purely random") ),
    n_ = paste0("n=",n)
  ) %>%
  ggplot(aes(x=p)) +
  facet_wrap(~n_) + 
  geom_line( 
      aes(y=value, 
          color=name_
          # color=fct_reorder(name, u_max)
        ), 
      linewidth=1) +
  # geom_line(data = . %>% filter((name %in% "random")), linewidth=1.5, linetype="longdash", aes(y=value, color="random")) +
  # geom_line(data = . %>% filter(!(name %in% "random")), 
  #           aes(y=value, color=fct_reorder(name, u_max)), linewidth=1) +
  # scale_color_manual(values=c("firebrick", "random"="dodgerblue2"), name="") +
  scale_color_manual(values=c(my_palette_n2, "green"), name="") +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("expected max Hamming score") 
plot_escore_nbrackets_2
ggsave(paste0("plot_thm1/plot_escore_nbrackets_2.png"), plot_escore_nbrackets_2, width=12, height=7)
 

###############################################
### number of U chalky brackets as a function of U ###
###############################################

plot_num_U_chalky_brackets = tibble(u_max = 0:m) %>%
  mutate(
    num = choose(m,u_max),
    num = cumsum(num),
    num = log(num, base=10),
  ) %>%
  ggplot(aes(x=u_max, y=num)) +
  # ylab("log#(U)") + 
  ylab(TeX("$\\log_{10}($#U)")) + 
  xlab("U") +
  scale_x_continuous(breaks=seq(0,m,by=5)) +
  # scale_y_continuous(breaks=seq(0,m,by=5)) +
  labs(title="number of U-chalky brackets") +
  geom_point(size=2) +
  geom_line(size=1)
plot_num_U_chalky_brackets
ggsave("plot_thm1/plot_num_U_chalky_brackets.png",plot_num_U_chalky_brackets,
      width=8, height=5)


plot_num_U_chalky_brackets_1 = tibble(u_max = 0:m) %>%
  mutate(
    num = choose(m,u_max),
    num = cumsum(num),
    num = log(num, base=10),
  ) %>%
  filter(u_max <= 5) %>%
  ggplot(aes(x=u_max, y=num)) +
  # ylab("log#(U)") + 
  ylab(TeX("$\\log_{10}($#U)")) + 
  xlab("U") +
  scale_x_continuous(breaks=seq(0,m,by=1)) +
  # scale_y_continuous(breaks=seq(0,m,by=5)) +
  labs(title="number of U-chalky brackets") +
  geom_point(size=2) +
  geom_line(size=1)
plot_num_U_chalky_brackets_1
ggsave("plot_thm1/plot_num_U_chalky_brackets_1.png",plot_num_U_chalky_brackets_1,
       width=8, height=5)


tibble(u_max = 0:m) %>%
  mutate(
    num = choose(m,u_max),
    num = cumsum(num)
  )

###############################################
### optimal U ###
###############################################

plot_U_star = 
  bind_rows(
    plot_df_escore_nChalkyBrackets %>% rename(escore=chalky),
    plot_df_escore_nRandomBrackets%>% rename(escore=random),
  ) %>% 
  mutate(u_max = replace_na(u_max, 63)) %>%
  filter(p >= 0.6) %>%
  group_by(p,n) %>%
  summarise(U_star = u_max[which( escore == max(escore) )]) %>%
  ungroup() %>%
  ggplot(aes(x=p, color=factor(n), y=U_star)) +
  geom_point() +
  geom_line() +
  xlab("p") + ylab(TeX("$U^{*}$")) +
  # labs(title=TeX("optimal chalkiness parameter $U^{*}(n,p)$")) +
  scale_color_manual(values = my_palette_n2, name="n")
plot_U_star
