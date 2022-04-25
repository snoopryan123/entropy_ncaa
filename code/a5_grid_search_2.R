source("a2_main.R")

#########################
### entropy quantiles ###
#########################

# saveRDS(quantile(r100k_ent$log_prob_x, seq(0,1,by=1/2)), "ent_quantile_2.rds")
# saveRDS(quantile(r100k_ent$log_prob_x, seq(0,1,by=1/4)), "ent_quantile_4.rds")
# saveRDS(quantile(r100k_ent$log_prob_x, seq(0,1,by=1/5)), "ent_quantile_5.rds")
# saveRDS(quantile(r100k_ent$log_prob_x, seq(0,1,by=1/7)), "ent_quantile_7.rds")
# saveRDS(quantile(r100k_ent$log_prob_x, seq(0,1,by=1/10)), "ent_quantile_10.rds")

ent_quantile_2 = readRDS("ent_quantile_2.rds")
ent_quantile_4 = readRDS("ent_quantile_4.rds")
ent_quantile_5 = readRDS("ent_quantile_5.rds")
ent_quantile_7 = readRDS("ent_quantile_7.rds")
ent_quantile_10 = readRDS("ent_quantile_10.rds")

#######################
### Overlap Metrics ###
#######################

d0_matrix <- function(bracket_set) {
  ### bracket_set is a tibble of brackets {x_i}
  d0 <- function(x1,x2) {
    sum(x1$team_idx - x2$team_idx != 0)
  }
  bs = unique(bracket_set$bracket_idx)
  lbs = length(bs)
  d0s = matrix(nrow=lbs,ncol=lbs)
  rownames(d0s) = bs
  colnames(d0s) = bs
  for (i in 1:(lbs-1)) { 
    for (j in (i+1):lbs) {
      #print(c(i,j))
      bi = bs[i]
      bj = bs[j]
      xi = bracket_set %>% filter(bracket_idx == bi)
      xj = bracket_set %>% filter(bracket_idx == bj)
      d0s[i,j] = d0(xi, xj)
    }
  }
  d0s
}

d1_2_matrix <- function(scores_tib) {
  ### scores_tib is a tibble of (b, f1, f2)
  d <- function(x1_score, x2_score) {
    abs(x1_score - x2_score)
  }
  f1s = scores_tib$f1
  f2s = scores_tib$f2
  lfs = length(f1s)
  d1s = matrix(nrow=lfs,ncol=lfs)
  d2s = matrix(nrow=lfs,ncol=lfs)
  for (i in 1:(lfs-1)) { 
    for (j in (i+1):lfs) {
      #print(c(i,j))
      f1i = f1s[i]
      f1j = f1s[j]
      f2i = f2s[i]
      f2j = f2s[j]
      d1s[i,j] = d(f1i, f1j)
      d2s[i,j] = d(f2i, f2j)
    }
  }
  list(d1s, d2s)
}

##################################
######### grid search 1 ##########
##################################

grid_search_2 <- function(ent_quantiles, n) {
  M = 10^4
  taus = simulate_brackets(M)
  LLL = (length(ent_quantiles)-1)
  
  f1_scores = numeric(LLL)
  f2_scores = numeric(LLL)
  max_d0s = numeric(LLL)
  max_d1s = numeric(LLL)
  max_d2s = numeric(LLL)
  avg_d0s = numeric(LLL)
  avg_d1s = numeric(LLL)
  avg_d2s = numeric(LLL)
  
  names(f1_scores) = ent_quantiles[1:LLL]; 
  names(f2_scores) = ent_quantiles[1:LLL]; 
  
  for (i in 1:LLL) {
      print("\n"); print(c(n,i,ent_quantiles)); print("\n");
    
      if (i==1) {
        hU = ent_quantiles[2]
        B_he = sample_brackets_h_range(n, hU=hU)
      } else {
        hL = ent_quantiles[i]
        B_he = sample_brackets_h_range(n, hL=hL)
      }
      B_he_scores = get_max_scores(B_he, true_brackets=taus, print_b=FALSE)
      
      f1_scores[i] = mean(B_he_scores[[1]]$f1)
      f2_scores[i] = mean(B_he_scores[[1]]$f2)
      
      d0_mat = d0_matrix(B_he) 
      d1_2_mats = d1_2_matrix(B_he_scores[[1]])
      d1_mat = d1_2_mats[[1]]
      d2_mat = d1_2_mats[[2]]
      
      max_d0s[i] = max(d0_mat, na.rm=TRUE)
      max_d1s[i] = max(d1_mat, na.rm=TRUE)
      max_d2s[i] = max(d2_mat, na.rm=TRUE)
      avg_d0s[i] = mean(d0_mat, na.rm=TRUE)
      avg_d1s[i] = mean(d1_mat, na.rm=TRUE)
      avg_d2s[i] = mean(d2_mat, na.rm=TRUE)
    
  }

  list(f1_scores=f1_scores, 
       f2_scores=f2_scores,
       max_d0s=max_d0s, max_d1s=max_d1s, max_d2s=max_d2s,
       avg_d0s=avg_d0s, avg_d1s=avg_d1s, avg_d2s=avg_d2s)
}

for (n in c(100)) { #c(100,10^3,10^4)) {
  for (k in c(2,4,5,7,10)) {
    print("!!!"); print(c(n,k)); print("!!!");
    ent_quantiles_k = readRDS(paste0("ent_quantile_",k,".rds"))
    gs2_k_n = grid_search_2(ent_quantiles_k, n)
    print(gs2_k_n)
    saveRDS(gs2_k_n, paste0("gs2_",n,"_",k,".rds"))
  }
}


# gs2_ex = grid_search_2(ent_quantile_2, 10)


################################
######### view output ##########
################################

# read RDS
gs2_100_2 = readRDS("gs2_100_2.rds")
gs2_100_4 = readRDS("gs2_100_4.rds")
gs2_100_5 = readRDS("gs2_100_5.rds")
gs2_100_7 = readRDS("gs2_100_7.rds")
gs2_100_10 = readRDS("gs2_100_10.rds")





