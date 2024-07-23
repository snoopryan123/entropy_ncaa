

source("b11_smm_main.R")

GRID = expand.grid(
  p = seq(0.5,1,by=0.1),
  q = seq(0,1,by=0.1),
  method = c("pb", "gpb", "combinatorics")
)
GRID

ns = c(1,5,10,100,1000,10000)
results = matrix(nrow=nrow(GRID), ncol=length(ns))
colnames(results) = paste0("n=",ns)
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  results[i,] = eMaxHammingScore(m, p=GRID$p[i], q=GRID$q[i], method=GRID$method[i], ns=ns)
}
GRID = bind_cols(GRID,results)
GRID

GRID1 = GRID %>%
  pivot_longer(cols=-c(p,q,method)) %>%
  rename(eMaxHammingScore = value) %>%
  mutate(n = as.numeric(str_sub(name, start=3))) %>%
  relocate(n, .after = q) %>%
  select(-name)
GRID1

write_csv(GRID1, paste0(output_folder,"plot_grid_eMaxHammingScore.csv"))


