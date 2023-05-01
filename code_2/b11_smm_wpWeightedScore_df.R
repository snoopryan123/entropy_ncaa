

source("b11_smm_main.R")

### GRIDS
GRID1 = expand.grid(
  p = 0.75,
  q = seq(0,1,by=0.05),
  r = seq(0.5,1,by=0.05),
  score_method = "Hamming"
) %>% as_tibble()
# GRID1

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
  q_cutoff = seq(2.5, 4.5, by=1),
  r_cutoff = seq(2.5, 4.5, by=1),
  score_method = "ESPN"
) %>% as_tibble()
GRID2

# version_=1
# fold_=30
# num_folds_=66

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
fold_ = as.numeric(args[1])
version_ = as.numeric(args[2])
num_folds_ = as.numeric(args[3])
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
  ns = 10^(0:3)
  ks = 10^(0:3)
  # ns = c(1,5,10,100,1000,10000)
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
  # ns = 10^(0:8)
  # ns = c(1,5,10,100,1000,10000)
  ns = c(1,10,100)
  ks = c(1,10,100)
} else {
  stop("this version_ has not yet been implemented")
}
df_folds_0 = tibble(fold = cut(1:nrow(GRID), breaks=num_folds_,labels=FALSE)) %>%
  mutate(i = row_number()) 
df_folds = df_folds_0 %>% filter(fold == fold_)
idxs = df_folds$i
print(paste0("idxs ", idxs))
GRID_OG = GRID_OG[idxs,]
GRID = GRID[idxs,]
GRID_OG
GRID

results = array(dim=c(length(ns),length(ks),nrow(GRID)))
dimnames(results)[[1]] = paste0("n=",ns)
dimnames(results)[[2]] = paste0("k=",ks)
dimnames(results)[[3]] = paste0("i=",1:nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  prs = c(GRID$p1[i], GRID$p2[i], GRID$p3[i], GRID$p4[i], GRID$p5[i], GRID$p6[i])
  qrs = c(GRID$q1[i], GRID$q2[i], GRID$q3[i], GRID$q4[i], GRID$q5[i], GRID$q6[i])
  rrs = c(GRID$r1[i], GRID$r2[i], GRID$r3[i], GRID$r4[i], GRID$r5[i], GRID$r6[i])
  
  results[i,] = wpMaxWeightedScore(m,qrs,rrs,prs,ns,ks,score_method=GRID$score_method[i],print_every_n=1000)
}
GRID_OG = bind_cols(GRID_OG,results)
GRID_OG

GRIDa = GRID_OG %>%
  pivot_longer(cols=starts_with("n=")) %>%
  rename(eMaxScore = value) %>%
  mutate(n = as.numeric(str_sub(name, start=3))) %>%
  relocate(eMaxScore, .before=score_method) %>%
  relocate(n, .after=eMaxScore) %>%
  select(-name)
GRIDa

write_csv(GRIDa, paste0(output_folder,"plot_grid_wpMaxScore_v",version_,"_fold",fold_,".csv"))


