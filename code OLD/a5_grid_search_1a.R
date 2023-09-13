source("a2_main.R")

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

grid_search_1a <- function(n) {
  h_set = 37:57
  epsilon_set = c(1:5)
  M = 10^4
  taus = simulate_brackets(M)
  
  f1_scores = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  f2_scores = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  max_d0s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  max_d1s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  max_d2s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  avg_d0s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  avg_d1s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  avg_d2s = matrix(nrow=length(h_set), ncol=length(epsilon_set))
  
  rownames(f1_scores) = h_set; colnames(f1_scores) = epsilon_set;
  rownames(f2_scores) = h_set; colnames(f2_scores) = epsilon_set;
  
  for (i in 1:length(h_set)) {
    for (j in 1:length(epsilon_set)) {
      h = h_set[i]
      e = epsilon_set[j]
      print("\n"); print(c(n,h,e)); print("\n");
      
      B_he = sample_brackets_h_range(n, hL=h-e, hU=h+e)
      B_he_scores = get_max_scores(B_he, true_brackets=taus, print_b=FALSE)
      
      f1_scores[i,j] = mean(B_he_scores[[1]]$f1)
      f2_scores[i,j] = mean(B_he_scores[[1]]$f2)
      
      d0_mat = d0_matrix(B_he) 
      d1_2_mats = d1_2_matrix(B_he_scores[[1]])
      d1_mat = d1_2_mats[[1]]
      d2_mat = d1_2_mats[[2]]
      
      max_d0s[i,j] = max(d0_mat, na.rm=TRUE)
      max_d1s[i,j] = max(d1_mat, na.rm=TRUE)
      max_d2s[i,j] = max(d2_mat, na.rm=TRUE)
      avg_d0s[i,j] = mean(d0_mat, na.rm=TRUE)
      avg_d1s[i,j] = mean(d1_mat, na.rm=TRUE)
      avg_d2s[i,j] = mean(d2_mat, na.rm=TRUE)
    }
  }

  list(f1_scores=f1_scores, 
       f2_scores=f2_scores,
       max_d0s=max_d0s, max_d1s=max_d1s, max_d2s=max_d2s,
       avg_d0s=avg_d0s, avg_d1s=avg_d1s, avg_d2s=avg_d2s)
}

gs1_100 = grid_search_1a(100)
saveRDS(gs1_100, "gs1a_100.rds")

# gs1a_1k = grid_search_1a(10^3)
# saveRDS(gs1a_1k, "gs1a_1k.rds")

# gs1_10k = grid_search_1a(10^4)
# saveRDS(gs1_10k, "gs1a_10k.rds")

##################################
######### view results ###########
##################################

gs1a_100 = readRDS("gs1a_100.rds")


