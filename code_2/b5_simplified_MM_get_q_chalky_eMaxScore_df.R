
source("b5_simplified_MM_main.R")

##### tibble of expected score as a function of (n,p,q) for q-chalky brackets ##### 
filename = "plot_thm1/df_eMaxScore_q_chalky_npq.csv"
if ( file.exists((filename)) ) { 
  result = numeric(nrow(plot_grid_npq))
  for (i in 1:nrow(plot_grid_npq)) {
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


