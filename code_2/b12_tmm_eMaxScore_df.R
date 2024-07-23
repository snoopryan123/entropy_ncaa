
source("b12_tmm_main.R")

# version_ = 1
# fold_ = 1
args = commandArgs(trailingOnly=TRUE)
version_ = as.numeric(args[1])
fold_ = as.numeric(args[2])
num_folds = 10 #FIXME
num_true_brackets = 100 #FIXME

GRID1 = expand.grid(
  p = seq(0.5,1,by=0.1),
  q = seq(0,1,by=0.1),
  # n = c(1,10,100,1000),
  n = 10^(0:5),
  scoring_method = "Hamming"
) %>% as_tibble()
GRID1

GRID2 = expand.grid(
  p = 0.75,
  qE = seq(0.5,1,by=0.05),
  qL = seq(0.5,1,by=0.05),
  q_cutoff = seq(1.5, 5.5, by=1),
  # n = 10^(0:5),
  n = 10^(0:4),
  scoring_method = "Hamming"
) %>% as_tibble()
GRID2

GRID3 = expand.grid(
  p = 0.75,
  # qE = seq(0,1,by=0.1),
  # qL = seq(0,1,by=0.1),
  qE = seq(0.5,1,by=0.05),
  qL = seq(0.5,1,by=0.05),
  q_cutoff = seq(1.5, 5.5, by=1),
  # n = c(1,5,10,100,1000,10000),
  # n = 10^(0:5),
  n = 10^(0:4),
  scoring_method = "ESPN"
) %>% as_tibble()
GRID3

#####################################
### Expected Maximum Score in TMM ###
#####################################

print(paste0("version_=",version_))
if (version_ == 1) {
  GRID_OG = GRID1
  GRID = GRID_OG %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = q, q2 = q, q3 = q, q4 = q, q5 = q, q6 = q,
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
      q6 = ifelse(6 < q_cutoff, qE, qL)
    ) %>%
    select(-c(p,qE,qL))
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
      q6 = ifelse(6 < q_cutoff, qE, qL)
    ) %>%
    select(-c(p,qE,qL))
  GRID
}

################################################################################

set.seed(1235124+fold_*23421) ### use fold_ to get a different random sample
results = numeric(nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  
  prs = c(GRID$p1[i], GRID$p2[i], GRID$p3[i], GRID$p4[i], GRID$p5[i], GRID$p6[i])
  qrs = c(GRID$q1[i], GRID$q2[i], GRID$q3[i], GRID$q4[i], GRID$q5[i], GRID$q6[i])
  true_backet_set = tmm_sample_n_brackets_prs(m=m,n=num_true_brackets,prs=prs)
  subd_backet_set = tmm_sample_n_brackets_prs(m=m,n=GRID$n[i],prs=qrs)
  
  results[i] = tmm_compute_max_score(subd_backet_set, true_backet_set, 
                                     scoring_method = GRID$scoring_method[i], expected_score = T)
}
if (version_ == 1 | version_ == 2) {
  GRID_OG$eMaxHammingScore = results
} else {
  GRID_OG$eMaxScore = results
}

write_csv(GRID_OG, paste0(output_folder,"plot_grid_eMaxScore_v",version_,"_fold",fold_,".csv"))


