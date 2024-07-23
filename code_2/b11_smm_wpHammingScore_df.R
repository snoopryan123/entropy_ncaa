

source("b11_smm_main.R")

GRID = expand.grid(
  p = 0.75,
  # q = seq(0,1,by=0.1),
  # r = seq(0.5,1,by=0.1)
  q = seq(0,1,by=0.05),
  r = seq(0.5,1,by=0.05)
)
GRID

# ns = c(1,5,10,100,1000,10000)
ns = 10^(0:8)
ks = 10^(0:8)
results = array(dim=c(length(ns),length(ks),nrow(GRID)))
dimnames(results)[[1]] = paste0("n=",ns)
dimnames(results)[[2]] = paste0("k=",ks)
dimnames(results)[[3]] = paste0("i=",1:nrow(GRID))
for (i in 1:nrow(GRID)) {
  print(paste0(i,"/",nrow(GRID)))
  results[,,i] = wpMaxHammingScore(m, q=GRID$q[i], r=GRID$r[i], p=GRID$p[i], ns=ns, ks=ks)
}

GRID1 = reshape2::melt(results) %>%
  as_tibble() %>%
  mutate(
    n = as.numeric(str_sub(Var1,start=3)),
    k = as.numeric(str_sub(Var2,start=3)),
    i = as.numeric(str_sub(Var3,start=3)),
  ) %>%
  rename(wp = value) %>%
  left_join( GRID %>% mutate(i = row_number()), by="i") %>%
  select(-c(Var1,Var2,Var3,i))
GRID1

write_csv(GRID1, paste0(output_folder,"plot_grid_wpHammingScore.csv"))


