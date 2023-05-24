
source("a0_loadStuff.R")

################
### Get Data ###
################

###
m = 63 ### number of games in March Madness

D = read_csv("../data/538_ELO.csv") %>% rename(game_id = initial_game_num) ##%>% arrange(game_id)
START_OF_TOURNEY = D %>% arrange(game_id) %>% select(team_idx, game_id, seed) %>% mutate(round = 1)
START_OF_TOURNEY

P_538_2022 = read.csv( paste0("../data/P_538_2022.csv"), row.names = 1, header= TRUE)
# P_538_2022 = read.csv( paste0("../data/P1.csv"), row.names = 1, header= TRUE)

# PROB_METHOD = "P_538_2022"
# PROB_METHOD = "P1"

P <- function(team_1s, team_2s, prob_method="P_538_2022", P_matrix=NULL) {
  if (prob_method == "P_538_2022") {
    ### team i beats team j according to ELO
    probs = P_538_2022[ cbind(team_1s, team_2s) ]
  } else if (prob_method == "custom_P_matrix") {
    if (is.null(P_matrix)) {
      stop(paste0("to use prob_method = ", "custom_P_matrix, you must specify P_matrix"))
    }
    ### team i beats team j according to ELO
    probs = P_matrix[ cbind(team_1s, team_2s) ]
  } else if (prob_method == "naive_chalky") {
    ### team i beats team j w.p. 0.90 if better seed
    seed_1s = (tibble(team_idx = team_1s) %>% left_join(START_OF_TOURNEY, by = "team_idx"))$seed
    seed_2s = (tibble(team_idx = team_2s) %>% left_join(START_OF_TOURNEY, by = "team_idx"))$seed
    probs = (seed_1s - seed_2s < -1)*0.9 + (abs(seed_1s - seed_2s) <= 1)*0.5 + (seed_1s - seed_2s > 1)*0.1
    # probs = (seed_1s - seed_2s < 0)*0.9 + (seed_1s - seed_2s == 0)*0.5 + (seed_1s - seed_2s > 0)*0.1
  } else if (prob_method == "naive_random") {
    probs = rep(0.5, length(team_1s))
  } else {
    stop("prob_method not implemented in P function")
  }
  return(probs)
}

P_mat <- function(prob_method="P_538_2022") {
  if (prob_method == "P_538_2022") {
    ### team i beats team j according to ELO
    probs = P_538_2022
  } else {
    stop(paste0("prob_method = ", prob_method, " is not implemented in P_mat function"))
  }
  return(probs)
}

###
P_file = "P_538_2022" # "P_538_2022" # "P1"
plot_folder = paste0("plot_", P_file, "/")
##output_folder = paste0("job_outpit",P_file, "/")

###########################
### Simulate N Brackets ###
###########################

### helper functions
odd <- function(x) x%%2 != 0
even <- function(x) x%%2 == 0
sample_winners <- function(probs) { as.numeric( runif(n = length(probs)) <= probs ) }

### sample N brackets
sample_n_brackets <- function(n, prob_method="P_538_2022", P_matrix=NULL, keep_probs=FALSE) {
  #####
  # n == number of brackets to be sampled
  # prob_method in {"P_538_2022", "naively_chalky"} 
  # determines the probability of team i beating team j
  #####
  tourney_n = list()
  tourney_n$n = n
  tourney_n$rd0_n = rep(START_OF_TOURNEY$team_idx, n)
  tourney_n$probs = keep_probs

  for (rd in 1:6) {
    curr_rd = tourney_n[[paste0("rd",rd-1,"_n")]]
    
    team_1s = curr_rd[odd(1:length(curr_rd))]
    team_2s = curr_rd[even(1:length(curr_rd))]
    probs_actual = P(team_1s, team_2s)
    probs_createBracket = P(team_1s, team_2s, prob_method=prob_method, P_matrix=P_matrix)
    if (keep_probs) {
      tourney_n[[paste0("probs_rd",rd,"_n")]] = probs_actual
      tourney_n[[paste0("probs_createBracket_rd",rd,"_n")]] = probs_createBracket
    }
    winners = sample_winners(probs_createBracket)
    
    next_rd = team_1s*winners + team_2s*(1-winners)
    tourney_n[[paste0("rd",rd,"_n")]] = next_rd
  }
  
  tourney_n$rd0_n <- NULL
  
  return(tourney_n)
}

# bracket_set_a = sample_n_brackets(n=5)
# bracket_set_d = sample_n_brackets(n=5, prob_method = "naive_chalky")
# bracket_set_b = sample_n_brackets(n=1e4)
# bracket_set_c = sample_n_brackets(n=1e5)

###########################
### Bracket Set Methods ###
###########################

extract_bracket <- function(bracket_set, i) {
  ###
  # create a new bracket set which contains just the i^th bracket from bracket_set
  ###
  bracket_set$n = 1
  for (rd in 1:6) {
    rd_size = 2^(6-rd)
    curr_rd = bracket_set[[paste0("rd",rd,"_n")]]
    bracket_i_idxs = (1:rd_size)+(i-1)*rd_size
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd[bracket_i_idxs]
  }
  return(bracket_set)
}

extract_brackets <- function(bracket_set, bracket_idxs) {
  ###
  # create a new bracket set which contains just the i^th bracket from bracket_set
  ###
  bracket_set$n = length(bracket_idxs)
  for (rd in 1:6) {
    rd_size = 2^(6-rd)
    curr_rd = bracket_set[[paste0("rd",rd,"_n")]]
  
    A = matrix(1:rd_size, nrow=length(bracket_idxs), ncol=rd_size, byrow = TRUE)
    B = matrix(bracket_idxs, nrow=length(bracket_idxs), ncol=rd_size)
    C = A + (B-1)*rd_size
    idxs = as.numeric(t(C))
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd[idxs]
    
    if (bracket_set$probs) {
      curr_probs_rd = bracket_set[[paste0("probs_rd",rd,"_n")]]
      bracket_set[[paste0("probs_rd",rd,"_n")]] = curr_probs_rd[idxs]
    }
  }
  return(bracket_set)
}

merge_brackets <- function(bracket_set_1, bracket_set_2) {
  ###
  # create a new bracket set which contains the brackets from bracket_set_1 and bracket_set_2
  ###
  bracket_set = list()
  bracket_set$n = bracket_set_1$n + bracket_set_2$n
  bracket_set$probs = bracket_set_1$probs & bracket_set_2$probs
  for (rd in 1:6) {
    rd_size = 2^(6-rd)
    curr1_rd = bracket_set_1[[paste0("rd",rd,"_n")]]
    curr2_rd = bracket_set_2[[paste0("rd",rd,"_n")]]
    curr_rd = c(curr1_rd, curr2_rd)
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd
    
    if (bracket_set$probs) {
      curr1_probs_rd = bracket_set_1[[paste0("probs_rd",rd,"_n")]]
      curr2_probs_rd = bracket_set_2[[paste0("probs_rd",rd,"_n")]]
      curr_probs_rd = c(curr1_probs_rd, curr2_probs_rd)
      bracket_set[[paste0("probs_rd",rd,"_n")]] = curr_probs_rd
    }
    
  }
  return(bracket_set)
}

###########################
###  Score bracket sets ###
###########################

compute_max_score <- function(submitted_brackets, true_brackets, scoring_method="ESPN", expected_score=F) {
  #####
  # submitted_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS submitted brackets
  # true_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS true brackets, for MoNUM_TRUE_BRACKETSe Carlo simulation
  # scoring_method in {"ESPN", "num_correct"} determines
  # the score f(x|tau) of a submitted bracket x relative to a true bracket tau
  #####
  NUM_TRUE_BRACKETS = true_brackets$n
  NUM_SUBD_BRACKETS = submitted_brackets$n
  scores_mat = matrix(nrow=NUM_TRUE_BRACKETS, ncol=NUM_SUBD_BRACKETS)
  
  for (tb in 1:NUM_TRUE_BRACKETS) {
    scores_tb = numeric(NUM_SUBD_BRACKETS)
    
    for (rd in 1:6) {
      rd_size = 2^(6-rd)
      true_rd = true_brackets[[paste0("rd",rd,"_n")]] [(1:rd_size)+(tb-1)*rd_size]
      true_rd = rep(true_rd, NUM_SUBD_BRACKETS)
      sub_rd = submitted_brackets[[paste0("rd",rd,"_n")]]
      
      if (scoring_method == "ESPN") {
        scores_rd = as.numeric(true_rd == sub_rd) * 2^(rd-1) * 10
      } else if (scoring_method == "num_correct") {
        scores_rd = as.numeric(true_rd == sub_rd)
      }
      
      scores_rd_mat = matrix(scores_rd, nrow = NUM_SUBD_BRACKETS, ncol = rd_size)
      scores_tb = scores_tb + rowSums(scores_rd_mat)
      
    }
    
    scores_mat[tb,] = scores_tb
  }
  row_maxes = apply(scores_mat, MARGIN=1, FUN=max)
  if (expected_score) {
    return( mean(row_maxes) )
  } else {
    return( row_maxes )
  }
}

# ### ex
# ex_true_backets = sample_n_brackets(n=100)
# 
# n = 100000
# ex_submitted_backets_elo = sample_n_brackets(n=n, prob_method = "P_538_2022")
# ex_submitted_backets_naiveChalky = sample_n_brackets(n=n, prob_method = "naive_chalky")
# ex_scores_elo = compute_max_score(ex_submitted_backets_elo, ex_true_backets, scoring_method="ESPN")
# ex_scores_naiveChalky = compute_max_score(ex_submitted_backets_naiveChalky, ex_true_backets, scoring_method="ESPN")
# ex_score_diffs = ex_scores_elo - ex_scores_naiveChalky
# ex_score_diffs = ex_score_diffs[ex_score_diffs != 0]
# mean(ex_score_diffs > 0)

#################################
### Entropy of a  bracket set ###
#################################

compute_entropies <- function(bracket_set) {
  #####
  # output a vector of entropies, one for each bracket in bracket_set
  #####
  NUM_BRACKETS = bracket_set$n
  entropies = numeric(NUM_BRACKETS)

  for (rd in 1:6) {
    rd_size = 2^(6-rd)
    probs_rd = bracket_set[[paste0("probs_rd",rd,"_n")]]
    if (is.null(probs_rd)) {
      stop("need to use `keep_probs=TRUE` in `sample_n_brackets`")
    }
    ents_rd = -log(probs_rd, base=2)
    ents_rd_mat = matrix(ents_rd, byrow = TRUE, nrow = NUM_BRACKETS, ncol = rd_size)
    entropies = entropies + rowSums(ents_rd_mat)
  }

  return(entropies)
}

plot_entropy_hist <- function(entropies, filename_str, title="",savePlot=TRUE) {
  entropy_hist = tibble(entropies) %>% ggplot() + 
    geom_histogram(
      aes(x=entropies, 
          # y=after_stat(density)
      ), fill="black",bins=100
    ) + 
    geom_vline(aes(xintercept = mean(entropies)), color="dodgerblue2") +
    # theme(
    #   axis.text.y=element_blank(),
    #   axis.ticks.y=element_blank()
    # ) +
    xlab("H(x)") +
    labs(title=title)
  entropy_hist
  if (savePlot) {
    ggsave(paste0(plot_folder, "entropy_hist_",filename_str,".png"), entropy_hist, width=8,height=7)
  } else {
    entropy_hist
  }
}

#####################
### Entropy Range ###
#####################

sample_n_brackets_entropyRange <- function(n, hL, hU, prob_method="P_538_2022") {
  bracket_set = NULL
  bracket_set_trunc = NULL
  while(TRUE) {
    ### sample some brackets
    bracket_set = sample_n_brackets(n=3*n, prob_method=prob_method, keep_probs=T) 
    ### keep sampled brackets that are within the entropy range (inclusive)
    entropies = compute_entropies(bracket_set)
    bracket_idxs_keep = which(hL <= entropies & entropies <= hU)
    bracket_set_trunc_i = extract_brackets(bracket_set, bracket_idxs_keep)
    ### merge newly sampled brackets with previous brackets
    if (is.null(bracket_set_trunc)) {
      bracket_set_trunc = bracket_set_trunc_i
    } else {
      bracket_set_trunc = merge_brackets(bracket_set_trunc, bracket_set_trunc_i)
    }
    ### make sure there are exactly n brackets
    if (bracket_set_trunc$n > n) {
      bracket_set_trunc = extract_brackets(bracket_set_trunc, 1:n)
    }
    ### end the loop once we have n brackets
    if (bracket_set_trunc$n >= n) {
      break
    }
    print(paste0("sampling: have found ", bracket_set_trunc$n, " brackets so far with hL=",hL, " and hU=", hU))
  }
  return(bracket_set_trunc)
}

# bracket_set = sample_n_brackets_entropyRange(n, 46, 49)
# entropies = compute_entropies(bracket_set)
# c(min(entropies), max(entropies))

#####################
### Entropy Range ###
#####################

Q_mat_chalkyLambda <- function(lambda, strat, prob_method="P_538_2022") {
  ### lambda is a real number between 0 and 1 (e.g., lambda=1/2)
  ### strat in {1,2} denotes which chalky lambda strategy to use
  ### return a matrix Q = Q{ij} 
  
  P_ = P_mat(prob_method = prob_method)
  P_upper = P_[upper.tri(P_, diag = FALSE)]
  ### check
  # all(P_upper >= 1/2)
  
  d <- function(p) {
    ### p is a vector of probabilities in [1/2, 1]
    ### e.g.,   p = c(1/2, 5/8, 3/4, 7/8, 1)
    temp = tibble(p - 1/2, 1 - p)
    4 * do.call(pmin, temp)
  }
  ### check
  # d(P_upper)
  # P_upper[1:10]
  # d(P_upper)[1:10]
  
  if (strat == 1) {
    Q_upper = (1-lambda)*P_upper + lambda*1
    ### check
    # all(Q_upper >= 1/2 & Q_upper <= 1)
  } else if (strat == 2) {
    lambda_d = lambda * d(P_upper)
    Q_upper = (1-lambda_d)*P_upper + lambda_d*1
    ### check
    # all(Q_upper >= 1/2 & Q_upper <= 1)
  } else {
    stop(paste0("strat = ", strat, " is not implemented in sample_n_brackets_chalkyLambda"))
  }
  ### check
  # lambda = 0.5 #1
  # P_upper_1 = seq(0.5,1,b=0.05)
  # Q_upper_1 = (1-lambda)*P_upper_1 + lambda*1
  # Q_upper_2 = (1-lambda*d(P_upper_1))*P_upper_1 + lambda*d(P_upper_1)*1
  # tibble(lambda, P_upper_1, Q_upper_1, Q_upper_2)
  
  Q_upper_ = matrix(nrow=nrow(P_), ncol=ncol(P_))
  Q_upper_[upper.tri(Q_upper_, diag = FALSE)] = Q_upper
  # Q_upper_[1:5,1:5] ### check
  Q_lower_ = 1 - t(Q_upper_)
  # Q_lower_[1:5,1:5] ### check
  Q_upper_[is.na(Q_upper_)] = 0
  Q_lower_[is.na(Q_lower_)] = 0
  diag(Q_upper_) = NA
  diag(Q_lower_) = NA
  Q_ = Q_upper_ + Q_lower_
  # Q_[1:5,1:5] ### check
  # (Q_ + t(Q_))[1:5,1:5] ### check

  # browser()
  
  return(Q_)
}
# ### check
# Q_mat_chalkyLambda(lambda=0.8, strat=1)[1:5,1:5]
# Q_mat_chalkyLambda(lambda=0.8, strat=2)[1:5,1:5]
# Q_mat_chalkyLambda(lambda=1, strat=1)[1:5,1:5]
# Q_mat_chalkyLambda(lambda=1, strat=2)[1:5,1:5]

sample_n_brackets_chalkyLambda <- function(n, lambda, strat, prob_method="P_538_2022") {
  ### n is a positive integer (e.g., n = 1000)
  ### lambda is a real number between 0 and 1 (e.g., lambda=1/2)
  ### strat in {1,2} denotes which chalky lambda strategy to use
  ### return n sampled brackets
  
  Q_ = Q_mat_chalkyLambda(lambda, strat, prob_method=prob_method)
  bracket_set = sample_n_brackets(n, prob_method="custom_P_matrix", P_matrix=Q_)
  # bracket_set = sample_n_brackets(n, prob_method="custom_P_matrix", P_matrix=Q_, keep_probs=TRUE) 
  return(bracket_set)
}

# Q_mat_chalkyLambda(lambda=1, strat=1)[1:5,1:5]
# sample_n_brackets_chalkyLambda(n=100, lambda=1, strat=1)
# Q_mat_chalkyLambda(lambda=0.8, strat=2)[1:5,1:5]
# sample_n_brackets_chalkyLambda(n=100, lambda=0.8, strat=2)
# Q_mat_chalkyLambda(lambda=0.2, strat=2)[1:5,1:5]
# sample_n_brackets_chalkyLambda(n=100, lambda=0.2, strat=2)
# Q_mat_chalkyLambda(lambda=0, strat=2)[1:5,1:5]
# sample_n_brackets_chalkyLambda(n=100, lambda=0, strat=2)

#########################
### Bracket Diverstiy ###
#########################

compute_bracket_diversity <- function(bracket_set, print_every_n=50, output_distmat=FALSE) {
  ###
  # compute the FN (min, mean, or max) of the
  # pairwise distances between each bracket in bracket_set
  # takes a long time!
  ###
  n = bracket_set$n
  dist_mat = matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    if (i %% print_every_n == 0) print(paste0(i, "/", n))
    j = 1
    while (j < i) {
      # print(c(i,j))
      bracket_i = extract_brackets(bracket_set, i)
      bracket_j = extract_brackets(bracket_set, j)
      dist_ij = m - compute_max_score(bracket_i, bracket_j, scoring_method="num_correct", expected_score=F)
      dist_mat[i,j] = dist_ij
      j = j+1
    }
  }
  
  if (output_distmat) {
    dist_mat
  } else {
    tibble(
      min = min(dist_mat, na.rm=T),
      mean = mean(dist_mat, na.rm=T),
      max = max(dist_mat, na.rm=T)
    )
  }
}

compute_bracket_diversity_1 <- function(bracket_set, bracket, print_every_n=50, output_distmat=FALSE) {
  ###
  # compute the FN (min, mean, or max) of the
  # distance between bracket and each bracket in bracket_set
  ###
  n = bracket_set$n
  dist_mat = numeric(n)
  for (i in 1:n) {
    if (i %% print_every_n == 0) print(paste0(i, "/", n))
    bracket_i = extract_brackets(bracket_set, i)
    dist_i0 = m - compute_max_score(bracket_i, bracket, scoring_method="num_correct", expected_score=F)
    dist_mat[i] = dist_i0
  }
  
  if (output_distmat) {
    dist_mat
  } else {
    tibble(
      min = min(dist_mat, na.rm=T),
      mean = mean(dist_mat, na.rm=T),
      max = max(dist_mat, na.rm=T)
    )
  }
}

sample_n_greedy_diverse_brackets <- function(n, prob_method="P_538_2022", 
                                             keep_probs=FALSE, print_every_n=50,
                                             entropy_range=NULL) {
  if (n > 1000) {
    stop("careful: this is an intensive procedure")
  }
  
  n0 = max(1000, 2*n) #FIXME
  if (is.null(entropy_range)) {
    bracket_set = sample_n_brackets(n0, prob_method=prob_method, keep_probs=keep_probs)
  } else {
    hL = entropy_range[1]
    hU = entropy_range[2]
    if (!is.numeric(hL) | !is.numeric(hU) | !(hL<hU)) {
      stop("`entropy_range` must be c(hL,hU) where hL,hU are numeric and hL<hU")
    }
    bracket_set = sample_n_brackets_entropyRange(n0, hL, hU)
  }
  ### get bracket diversity matrix, and modify it
  bracket_diversity = compute_bracket_diversity(bracket_set, print_every_n=print_every_n, output_distmat=TRUE) 
  bracket_diversity_1 = ifelse(is.na(bracket_diversity), 0, bracket_diversity)
  bracket_diversity_2 = bracket_diversity_1 + t(bracket_diversity_1)
  diag(bracket_diversity_2) = NA
  
  ### greedy algorithm: at each step, select the bracket which has largest min. distance to all other remaining brackets
  DISTMAT = bracket_diversity_2
  rownames(DISTMAT) = 1:nrow(DISTMAT)
  colnames(DISTMAT) = 1:ncol(DISTMAT)
  ### initialize by selecting the first bracket
  selected_bracket_idxs = c(1)
  DISTMAT = DISTMAT[,2:ncol(DISTMAT)]
  
  for (i in 2:n) {
    distmat_i = DISTMAT[selected_bracket_idxs,]
    ### for each remaining bracket (cols), compute its min. distance to all the selected brackets (rows)
    if (i == 2) {
      min_dists_i = distmat_i
    } else {
      min_dists_i = apply(distmat_i, MARGIN=2, FUN=min) ### min
    }
    ### select the remaining bracket (col) which has largest min. dist to the already selected brackets
    selected_bracket_idx = as.numeric(names(  which(min_dists_i == max(min_dists_i))[1]  ))
    selected_bracket_idxs = c(selected_bracket_idxs, selected_bracket_idx)
    ### remove columns of the brackets which have already been selected;
    ### so that we don't select the same bracket twice
    col_idx_to_remove = which(as.numeric(colnames(DISTMAT)) == selected_bracket_idx)
    remaining_col_idxs = setdiff(1:ncol(DISTMAT), col_idx_to_remove)
    DISTMAT = DISTMAT[, remaining_col_idxs]
  }
  
  ### 
  extract_brackets(bracket_set, selected_bracket_idxs)
}

sample_n_delta_diverse_brackets <- function(n, n0, delta, prob_method="P_538_2022", 
                                             keep_probs=FALSE, print_every_n=50,
                                             entropy_range=NULL) {
  ###
  # n is the number of brackets to sample with delta diversity
  # n0 is the larger pool of brackets initially sampled prior to selecting delta diverse brackets
  # delta is the bracket diversity parameter
  ###
  
  if (n > 1000) {
    stop("careful: this is an intensive procedure")
  }
  
  n0 = max(n0, max(1000, 2*n))    #FIXME
  if (is.null(entropy_range)) {
    bracket_set = sample_n_brackets(n0, prob_method=prob_method, keep_probs=keep_probs)
  } else {
    hL = entropy_range[1]
    hU = entropy_range[2]
    if (!is.numeric(hL) | !is.numeric(hU) | !(hL<hU)) {
      stop("`entropy_range` must be c(hL,hU) where hL,hU are numeric and hL<hU")
    }
    bracket_set = sample_n_brackets_entropyRange(n0, hL, hU)
  }
  
  selected_bracket_set = extract_bracket(bracket_set, 1)

  i = 2
  while (i <= n0) {
    bracket_i = extract_bracket(bracket_set, i)
    bracket_dist_i = compute_bracket_diversity_1(selected_bracket_set, bracket_i, print_every_n=print_every_n)$min
    if (bracket_dist_i >= delta) {
      ### if exceeds bracket diversity threshold, select the bracket
      selected_bracket_set = merge_brackets(selected_bracket_set, bracket_i)
    } 
    if (selected_bracket_set$n >= n) {
      break
    }
    if (i == n0) {
      ### need new bracket set
      print("sampling a new bracket set...")
      i = 1
      #FIXME
      
      if (is.null(entropy_range)) {
        bracket_set = sample_n_brackets(n0, prob_method=prob_method, keep_probs=keep_probs)
      } else {
        hL = entropy_range[1]
        hU = entropy_range[2]
        if (!is.numeric(hL) | !is.numeric(hU) | !(hL<hU)) {
          stop("`entropy_range` must be c(hL,hU) where hL,hU are numeric and hL<hU")
        }
        bracket_set = sample_n_brackets_entropyRange(n0, hL, hU)
      }
      
    } else {
      i = i + 1
    }
  }
  
  # if (selected_bracket_set$n < n) {
  #   stop("need to increase n0")
  # }
  
  return(selected_bracket_set)
}

