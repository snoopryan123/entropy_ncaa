
source("b12_tmm_main.R")

# version_ = 1
# fold_ = 1
args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
fold_ = as.numeric(args[2])
num_folds = 10 #FIXME
num_true_brackets = 100 #FIXME

GRID1 = expand.grid(
  p = 0.75,
  # q = seq(0,1,by=0.1),
  # r = seq(0.5,1,by=0.1)
  q = seq(0,1,by=0.05),
  r = seq(0.5,1,by=0.05),
  n = c(1, 10, 100, 1000),
  k = c(10, 100),
  # n = c(10, 100),
  # k = c(10, 100),
  # n = 10^(0:4),
  # k = 10^(0:4),
  # n = 10^(0:8),
  # k = 10^(0:8),
  scoring_method = "Hamming"
) %>% as_tibble() #%>% filter(n == k)
GRID1

GRID2 = expand.grid(
  p = 0.75,
  # qE = seq(0.5,1,by=0.05),
  # qL = seq(0.5,1,by=0.05),
  # rE = seq(0.5,1,by=0.05),
  # rL = seq(0.5,1,by=0.05),
  qE = seq(0.5,1,by=0.1),
  qL = seq(0.5,1,by=0.1),
  rE = seq(0.5,1,by=0.1),
  rL = seq(0.5,1,by=0.1),
  # q_cutoff = seq(1.5, 5.5, by=1),
  # r_cutoff = seq(1.5, 5.5, by=1),
  # q_cutoff = seq(2.5, 4.5, by=1),
  # r_cutoff = seq(2.5, 4.5, by=1),
  q_cutoff = 3.5,
  r_cutoff = 3.5,
  n = c(10, 100),
  k = c(10, 100),
  scoring_method = "Hamming"
) %>% as_tibble() %>% filter(n == k)
GRID2

GRID3 = expand.grid(
  p = 0.75,
  # qE = seq(0.5,1,by=0.05),
  # qL = seq(0.5,1,by=0.05),
  # rE = seq(0.5,1,by=0.05),
  # rL = seq(0.5,1,by=0.05),
  qE = seq(0.5,1,by=0.1),
  qL = seq(0.5,1,by=0.1),
  rE = seq(0.5,1,by=0.1),
  rL = seq(0.5,1,by=0.1),
  # q_cutoff = seq(1.5, 5.5, by=1),
  # r_cutoff = seq(1.5, 5.5, by=1),
  # q_cutoff = seq(2.5, 4.5, by=1),
  # r_cutoff = seq(2.5, 4.5, by=1),
  q_cutoff = 3.5,
  r_cutoff = 3.5,
  n = c(10, 100),
  k = c(10, 100),
  scoring_method = "ESPN"
) %>% as_tibble() %>% filter(n == k)
GRID3

##############################
### Win Probability in TMM ###
##############################

print(paste0("version_=",version_))
if (version_ == 1) {
  GRID_OG = GRID1
  GRID = GRID_OG %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = q, q2 = q, q3 = q, q4 = q, q5 = q, q6 = q,
      r1 = r, r2 = r, r3 = r, r4 = r, r5 = r, r6 = r,
    ) %>%
    select(-c(p,q))
  GRID
} else if (version_ == 2) {
  GRID_OG = GRID2
  GRID = GRID_OG %>%
    as_tibble() %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = ifelse(1 < q_cutoff, qE, qL),
      q2 = ifelse(2 < q_cutoff, qE, qL),
      q3 = ifelse(3 < q_cutoff, qE, qL),
      q4 = ifelse(4 < q_cutoff, qE, qL),
      q5 = ifelse(5 < q_cutoff, qE, qL),
      q6 = ifelse(6 < q_cutoff, qE, qL),
      r1 = ifelse(1 < r_cutoff, rE, rL),
      r2 = ifelse(2 < r_cutoff, rE, rL),
      r3 = ifelse(3 < r_cutoff, rE, rL),
      r4 = ifelse(4 < r_cutoff, rE, rL),
      r5 = ifelse(5 < r_cutoff, rE, rL),
      r6 = ifelse(6 < r_cutoff, rE, rL)
    ) %>%
    select(-c(p,qE,qL,rE,rL))
  GRID
} else if (version_ == 3) {
  GRID_OG = GRID3
  GRID = GRID_OG %>%
    as_tibble() %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = ifelse(1 < q_cutoff, qE, qL),
      q2 = ifelse(2 < q_cutoff, qE, qL),
      q3 = ifelse(3 < q_cutoff, qE, qL),
      q4 = ifelse(4 < q_cutoff, qE, qL),
      q5 = ifelse(5 < q_cutoff, qE, qL),
      q6 = ifelse(6 < q_cutoff, qE, qL),
      r1 = ifelse(1 < r_cutoff, rE, rL),
      r2 = ifelse(2 < r_cutoff, rE, rL),
      r3 = ifelse(3 < r_cutoff, rE, rL),
      r4 = ifelse(4 < r_cutoff, rE, rL),
      r5 = ifelse(5 < r_cutoff, rE, rL),
      r6 = ifelse(6 < r_cutoff, rE, rL)
    ) %>%
    select(-c(p,qE,qL,rE,rL))
  GRID
}

################################################################################

set.seed(1235124+fold_*23421) ### use fold_ to get a different random sample
results = numeric(nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  
  prs = c(GRID$p1[i], GRID$p2[i], GRID$p3[i], GRID$p4[i], GRID$p5[i], GRID$p6[i])
  qrs = c(GRID$q1[i], GRID$q2[i], GRID$q3[i], GRID$q4[i], GRID$q5[i], GRID$q6[i])
  rrs = c(GRID$r1[i], GRID$r2[i], GRID$r3[i], GRID$r4[i], GRID$r5[i], GRID$r6[i])
  true_backet_set = tmm_sample_n_brackets_prs(m=m,n=num_true_brackets,prs=prs)
  our_backet_set = tmm_sample_n_brackets_prs(m=m,n=GRID$n[i],prs=qrs)
  opp_backet_set = tmm_sample_n_brackets_prs(m=m,n=GRID$k[i],prs=rrs)
  
  our_scores = tmm_compute_max_score(our_backet_set, true_backet_set, 
                                    scoring_method = GRID$scoring_method[i])
  opp_scores = tmm_compute_max_score(opp_backet_set, true_backet_set, 
                                     scoring_method = GRID$scoring_method[i])
  results[i] = mean( as.numeric(our_scores >= opp_scores) )
}
GRID_OG$wp = results

write_csv(GRID_OG, paste0(output_folder,"plot_grid_wpScore_v",version_,"_fold",fold_,".csv"))


