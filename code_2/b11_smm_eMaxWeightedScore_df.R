

source("b11_smm_main.R")

### GRIDS
GRID1 = expand.grid(
  p = seq(0.5,1,by=0.1),
  q = seq(0,1,by=0.1),
  score_method = "Hamming"
) %>% as_tibble()
# GRID1

GRID2 = expand.grid(
  p = 0.75,
  # qE = seq(0,1,by=0.1),
  # qL = seq(0,1,by=0.1),
  qE = seq(0.5,1,by=0.05),
  qL = seq(0.5,1,by=0.05),
  score_method = "ESPN"
) %>% as_tibble()
# GRID2

# version_=1
# fold_=30
# num_folds_parralelization_=66

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
fold_ = as.numeric(args[1])
version_ = as.numeric(args[2])
num_folds_parralelization_ = as.numeric(args[3])
if (version_ == 1) {
  GRID_OG = GRID1
  GRID = GRID_OG %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = q, q2 = q, q3 = q, q4 = q, q5 = q, q6 = q,
    ) %>%
    select(-c(p,q))
  ns = c(1,5,10,100,1000,10000)
} else if (version_ == 2) {
  GRID_OG = GRID2
  GRID = GRID_OG %>%
    as_tibble() %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = qE, q2 = qE, q3 = qE, q4 = qL, q5 = qL, q6 = qL,
    ) %>%
    select(-c(p,qE,qL))
  ns = 10^(0:8)
  # ns = c(1,5,10,100,1000,10000)
} else {
  stop("this version_ has not yet been implemented")
}
df_folds_0 = tibble(fold = cut(1:nrow(GRID), breaks=num_folds_parralelization_,labels=FALSE)) %>%
  mutate(i = row_number()) 
df_folds = df_folds_0 %>%
  filter(fold == fold_)
idx_lower = df_folds$i[1]
idx_upper = df_folds$i[2]
print(paste0("idx ", c(idx_lower, idx_upper)))
GRID_OG = GRID_OG[idx_lower:idx_upper,]
GRID = GRID[idx_lower:idx_upper,]
GRID_OG
GRID

results = matrix(nrow=nrow(GRID), ncol=length(ns))
colnames(results) = paste0("n=",ns)
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  prs = c(GRID$p1[i], GRID$p2[i], GRID$p3[i], GRID$p4[i], GRID$p5[i], GRID$p6[i])
  qrs = c(GRID$q1[i], GRID$q2[i], GRID$q3[i], GRID$q4[i], GRID$q5[i], GRID$q6[i])
  
  results[i,] = eMaxWeightedScoreByRound_gpb(m,prs,qrs,ns,score_method=GRID$score_method[i],print_every_n=1000)
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

write_csv(GRIDa, paste0(output_folder,"plot_grid_eMaxScore_v",version_,"_fold",fold_,".csv"))


