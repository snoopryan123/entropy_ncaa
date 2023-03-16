
source("b5_simplified_MM_main.R")

##### tibbles containing expected score as a function of (n,p) for chalky brackets ##### 

# UMAX = 3 #FIXME #3 #10
# for (UMAX in 1:25) {
for (UMAX in 26:63) {
  ### takes 5 minutes
  plot_df_escore_nChalkyBrackets =
    plot_grid %>%
    rowwise() %>%
    mutate(
      chalky = escore_Mn_chalky(p,n,u_max=UMAX),
    )
  write_csv(plot_df_escore_nChalkyBrackets, paste0("plot_thm1/plot_df_escore_nChalkyBrackets_", "u_max=",UMAX,".csv"))
}

