

source("b11_smm_main.R")

### GRIDS
GRID1 = expand.grid(
  p = seq(0.5,1,by=0.1),
  q = seq(0,1,by=0.1),
  score_method = "Hamming"
)
GRID1

### since this is so computationally intensive, split into folds
args = commandArgs(trailingOnly=TRUE)
fold_ = as.numeric(args[1])
version_ = as.numeric(args[2])
num_folds_parralelization_ = as.numeric(args[3])
if (version_ == 1) {
  GRID =
    GRID1 %>%
    mutate(
      p1 = p, p2 = p, p3 = p, p4 = p, p5 = p, p6 = p,
      q1 = q, q2 = q, q3 = q, q4 = q, q5 = q, q6 = q,
    ) %>%
    select(-c(p,q))
  ns = c(1,5,10,100,1000,10000)
} else if (version_ == 2) {
  # GRID = plot_grid_maxEspnScore_SMM_v2
} else {
  stop("this version_ has not yet been implemented")
}
idxs_lower = nrow(GRID)/NUM_FOLDS_WPDF_PARALLELIZATION * (fold_-1)
idxs_upper = nrow(GRID)/NUM_FOLDS_WPDF_PARALLELIZATION * fold_
GRID = GRID[floor(idxs_lower):ceiling(idxs_upper),]

results = matrix(nrow=nrow(GRID), ncol=length(ns))
colnames(results) = paste0("n=",ns)
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  prs = c(GRID$p1[i], GRID$p2[i], GRID$p3[i], GRID$p4[i], GRID$p5[i], GRID$p6[i])
  qrs = c(GRID$q1[i], GRID$q2[i], GRID$q3[i], GRID$q4[i], GRID$q5[i], GRID$q6[i])
  
  results[i,] = eMaxWeightedScoreByRound_gpb(m,prs,qrs,ns,score_method=GRID$score_method[i],print_every_n=1000)
}
GRID = bind_cols(GRID,results)
GRID

GRIDa = GRID %>%
  pivot_longer(cols=-c(p1,p2,p3,p4,p5,p6,q1,q2,q3,q4,q5,q6,score_method)) %>%
  rename(eMaxScore = value) %>%
  mutate(n = as.numeric(str_sub(name, start=3))) %>%
  select(-name)
GRIDa

write_csv(GRIDa, paste0(output_folder,"plot_grid_eMaxScore_v",version_,"_fold",fold_,".csv"))


