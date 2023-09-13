source("a2_main.R")

#####################################################
### random sampling vs. forcing bracket diveristy ###
########### within the same entropy range ####$######
#####################################################



#######################
### Overlap Metrics ###
#######################

d0_vector <- function(bracket, bracket_set) {
  ### bracket_set is a tibble of brackets {x_i}
  d0 <- function(x1,x2) {
    sum(x1$team_idx - x2$team_idx != 0)
  }
  bs = unique(bracket_set$bracket_idx)
  lbs = length(bs)
  d0s = numeric(lbs)
  names(d0s) = bs
  for (i in 1:lbs) { 
    # if (i%%200 == 0) print(i)
    bi = bs[i]
    xi = bracket_set %>% filter(bracket_idx == bi)
    d0s[i] = d0(xi, bracket)
  }
  d0s
}

d12_vector <- function(curr_f1_score, all_f1_scores) {
  lfs = length(all_f1_scores)
  d1s = numeric(lfs)
  for (i in 1:lfs) { 
    d1s[i] = abs(all_f1_scores[i] - curr_f1_score)
  }
  d1s
}



#######################
###  ###
#######################

get_diverse_brackets_0a <- function(n, hL=-Inf, hu=Inf, d_metric="d0") {
  mm = 10^3 #10
  all_brackets = sample_brackets_h_range(mm, hL=hL, hU=hU)
  d0_mat = matrix(nrow = mm, ncol = mm)
  
  if (d_metric == "d1" | d_metric == "d2") {
    taus_ = simulate_brackets(25)
    all_f1_scores = numeric()
    all_f2_scores = numeric()
    for (i in 1:mm) {
      print(paste0("************* ", i))
      bi = all_brackets %>% filter(bracket_idx == unique(all_brackets$bracket_idx)[i])
      scores_i = get_max_scores(bi, true_brackets=taus_, print_b=TRUE)
      all_f1_scores = c(all_f1_scores, mean(scores_i[[1]]$f1))
      all_f2_scores = c(all_f2_scores, mean(scores_i[[1]]$f2))
    }
  }
  
  curr_bracket_idx = all_brackets$bracket_idx[1]
  curr_bracket = all_brackets %>% filter(bracket_idx == curr_bracket_idx)
  B_idxs = c(curr_bracket_idx)
  B = tibble(curr_bracket)
  remaining_brackets = all_brackets %>% filter(bracket_idx != curr_bracket_idx)
  selected_bracket_distances = numeric()
  
  for (i in 1:(n-1)) {
    print(paste0("~~~ ","i=",i," ~~~")); 
    
    # distances from current bracket to other brackets
    if (d_metric == "d0") {
      d0_vec = d0_vector(curr_bracket, all_brackets)
    } else if (d_metric == "d1") {
      curr_f1_score = all_f1_scores[curr_bracket_idx]
      d0_vec = d12_vector(curr_f1_score, all_f1_scores)
    } else if (d_metric == "d2") {
      curr_f2_score = all_f2_scores[curr_bracket_idx]
      d0_vec = d12_vector(curr_f2_score, all_f2_scores)
    }
    
    d0_vec[B_idxs] = -Inf
    d0_mat[,curr_bracket_idx] = unname(d0_vec)
    d0_mat_filled = d0_mat[, colSums(is.na(d0_mat)) != nrow(d0_mat)]
    
    if (i == 1) { 
      ### choose the bracket with largest distance from current bracket
      d0_vec_filled = d0_mat_filled 
    } else { 
      # colnames(d0_mat_filled) = B_idxs
      
      # ### choose the bracket whose sum of distances from previous brackets is largest
      # d0_vec_filled = rowSums(d0_mat_filled) 
      
      ### choose the bracket whose MIN distance from previous brackets is largest
      d0_vec_filled = apply(d0_mat_filled, MARGIN=1, FUN=min)
      print(paste0("$$$ ", max(d0_vec_filled), " $$$"))
    }     
    ###curr_bracket_idx = as.numeric( sample(which(d0_vec_filled == max(d0_vec_filled)), 1) ) ### weird behavior with SAMPLE function...
    curr_bracket_idx = which(d0_vec_filled == max(d0_vec_filled))[1]
    
    if (curr_bracket_idx %in% B_idxs) {
      print("CONFLICT.")
      print(curr_bracket_idx)
      print(B_idxs)
      browser()
    }
    
    selected_bracket_distances = c(selected_bracket_distances, max(d0_vec_filled)/i )

    curr_bracket = all_brackets %>% filter(bracket_idx == curr_bracket_idx)
    B_idxs = c(B_idxs, curr_bracket_idx)
    B = bind_rows(B, curr_bracket)
    remaining_brackets = remaining_brackets %>% filter(bracket_idx != curr_bracket_idx)
  }
  
  list(B, selected_bracket_distances)
}

#######################
###  ###
#######################

n = 100
M = 200#200#10^4
taus = simulate_brackets(M)


hL0 = -Inf #35
hU0 = 38 #45

B0 = sample_brackets_h_range(n, hL=hL0, hU=hU0)
# B1 = sample_brackets_h_range(n, hL=hL1, hU=hU1)

gdb_0 = get_diverse_brackets_0a(n, hL=hL0, hu=hU0, d_metric="d0")
D0 = gdb_0[[1]]
D0_selected_bracket_distances = gdb_0[[2]]

gdb_1 = get_diverse_brackets_0a(n, hL=hL0, hu=hU0, d_metric="d1")
D1 = gdb_1[[1]]
D1_selected_bracket_distances = gdb_1[[2]]

gdb_2 = get_diverse_brackets_0a(n, hL=hL0, hu=hU0, d_metric="d2")
D2 = gdb_2[[1]]
D2_selected_bracket_distances = gdb_2[[2]]

# D0 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count)
# sum( (D0 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count))$count != 63 )
# length(unique(D0$bracket_idx))
# 
# D1 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count)
# sum( (D1 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count))$count != 63 )
# length(unique(D1$bracket_idx))
# 
# D2 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count)
# sum( (D2 %>% group_by(bracket_idx) %>% summarise(count=n()) %>% arrange(-count))$count != 63 )
# length(unique(D2$bracket_idx))




B0_scores = get_max_scores(B0, true_brackets=taus, print_b=TRUE)
# B1_scores = get_max_scores(B1, true_brackets=taus, print_b=TRUE)
D0_scores = get_max_scores(D0, true_brackets=taus, print_b=TRUE)
D1_scores = get_max_scores(D1, true_brackets=taus, print_b=TRUE)
D2_scores = get_max_scores(D2, true_brackets=taus, print_b=TRUE)


mean(B0_scores[[1]]$f1)
mean(D0_scores[[1]]$f1)
mean(D1_scores[[1]]$f1)
mean(D2_scores[[1]]$f1)

mean(B0_scores[[1]]$f2)
mean(D0_scores[[1]]$f2)
mean(D1_scores[[1]]$f2)
mean(D2_scores[[1]]$f2)


### check bracket diversity 

d0_mat_B0 = d0_matrix(B0) 
d0_mat_D0 = d0_matrix(D0) 
max(d0_mat_B0, na.rm=TRUE)
max(d0_mat_D0, na.rm=TRUE)
mean(d0_mat_B0, na.rm=TRUE)
mean(d0_mat_D0, na.rm=TRUE)


d12_mats_B0 = d1_2_matrix(B0_scores[[1]])
d1_mat_B0 = d12_mats_B0[[1]]
d12_mats_D1 = d1_2_matrix(D1_scores[[1]])
d1_mat_D1 = d12_mats_D1[[1]]
max(d1_mat_B0, na.rm=TRUE)
max(d1_mat_D1, na.rm=TRUE)
mean(d1_mat_B0, na.rm=TRUE)
mean(d1_mat_D1, na.rm=TRUE)



d2_mat_B0 = d12_mats_B0[[2]]
d12_mats_D2 = d1_2_matrix(D2_scores[[1]])
d2_mat_D2 = d12_mats_D2[[2]]
max(d2_mat_B0, na.rm=TRUE)
max(d2_mat_D2, na.rm=TRUE)
mean(d2_mat_B0, na.rm=TRUE)
mean(d2_mat_D2, na.rm=TRUE)






