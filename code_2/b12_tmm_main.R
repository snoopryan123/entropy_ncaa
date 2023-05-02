
#########################################################
### Tree March Madness:
### n submitted Ber(q) brackets vs. 1 true Ber(p) bracket
### results of round rd-1 carry thru to round rd
#########################################################

source("a0_loadStuff.R")

output_folder = "plot_12/"
m = 63 ### number of games in the NCAA tournament, one below a power of 2 

###########################
### Simulate N Brackets ###
###########################

### sample N brackets using a constant p for each bit
tmm_sample_n_brackets_p <- function(m,n,p) {
  #####
  # n == number of bitstrings to be sampled
  # p == probability of a 1
  #####
  # browser()
  R = log(m+1,base=2)
  if (R %% 1 != 0) stop("m+1 must be a power of 2")
  tourney = list()
  tourney$m = m
  tourney$R = R
  tourney$n = n
  tourney$p = p

  for (rd in 1:tourney$R) {
    m_rd = 2^((R-rd))
    bitstring_rd = rbinom(n=m_rd*n, size=1, p=p)
    tourney[[paste0("rd",rd,"_n")]] = bitstring_rd
    # tourney[[paste0("probs_rd",rd,"_n")]] = probs_actual
  }

  return(tourney)
}

# bracket_set_a = tmm_sample_n_brackets_p(m=m,n=5,p=0.75)

### sample N brackets using a constant p for each bit within each round
tmm_sample_n_brackets_prs <- function(m,n,prs) {
  #####
  # n == number of bitstrings to be sampled
  # prs == c(p1,p2,...,pR) == probability of a 1 in each round
  #####
  # browser()
  R = log(m+1,base=2)
  if (R %% 1 != 0) stop("m+1 must be a power of 2")
  if (length(prs) != R) stop("prs must be a vector of length R")
  tourney = list()
  tourney$m = m
  tourney$R = R
  tourney$n = n
  tourney$prs = prs
  
  for (rd in 1:tourney$R) {
    p = prs[rd]
    m_rd = 2^((R-rd))
    bitstring_rd = rbinom(n=m_rd*n, size=1, p=p)
    tourney[[paste0("rd",rd,"_n")]] = bitstring_rd
    # tourney[[paste0("probs_rd",rd,"_n")]] = probs_actual
  }
  
  return(tourney)
}

# bracket_set_aa = tmm_sample_n_brackets_prs(m=m,n=5,prs=c(0.6,0.7,0.75,0.8,0.85,0.9))

###########################
### Bracket Set Methods ###
###########################

tmm_extract_bracket <- function(bracket_set, i) {
  ###
  # create a new bracket set which contains just the i^th bracket from bracket_set
  ###
  bracket_set$n = 1
  R = bracket_set$R
  for (rd in 1:R) {
    m_rd = 2^(R-rd)
    curr_rd = bracket_set[[paste0("rd",rd,"_n")]]
    bracket_i_idxs = (1:m_rd)+(i-1)*m_rd
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd[bracket_i_idxs]
  }
  return(bracket_set)
}

# bracket_set_a = tmm_sample_n_brackets_p(m=m,n=5,p=0.75)
# bracket_set_a2 = tmm_extract_bracket(bracket_set_a, 2) 

tmm_extract_brackets <- function(bracket_set, bracket_idxs) {
  ###
  # create a new bracket set which contains just the bracket_idxs^th brackets from bracket_set
  ###
  bracket_set$n = length(bracket_idxs)
  R = bracket_set$R
  for (rd in 1:R) {
    m_rd = 2^(R-rd)
    curr_rd = bracket_set[[paste0("rd",rd,"_n")]]
  
    A = matrix(1:m_rd, nrow=length(bracket_idxs), ncol=m_rd, byrow = TRUE)
    B = matrix(bracket_idxs, nrow=length(bracket_idxs), ncol=m_rd)
    C = A + (B-1)*m_rd
    idxs = as.numeric(t(C))
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd[idxs]
    
    # if (bracket_set$probs) {
    #   curr_probs_rd = bracket_set[[paste0("probs_rd",rd,"_n")]]
    #   bracket_set[[paste0("probs_rd",rd,"_n")]] = curr_probs_rd[idxs]
    # }
  }
  return(bracket_set)
}

# bracket_set_a = tmm_sample_n_brackets_p(m=m,n=5,p=0.75)
# bracket_set_a12 = tmm_extract_brackets(bracket_set_a, 1:2)

tmm_merge_brackets_p <- function(bracket_set_1, bracket_set_2) {
  ###
  # create a new bracket set which contains the brackets from bracket_set_1 and bracket_set_2
  ###
  bracket_set = list()
  if (bracket_set_1$p != bracket_set_2$p) stop("bracket_set merge not supported if the p's are not the same")
  if (bracket_set_1$R != bracket_set_2$R) stop("bracket_set merge not supported if the R's are not the same")
  bracket_set$m = bracket_set_1$m
  bracket_set$R = bracket_set_1$R
  bracket_set$n = bracket_set_1$n + bracket_set_2$n
  bracket_set$p = bracket_set_1$p
  for (rd in 1:bracket_set$R) {
    m_rd = 2^(6-rd)
    curr1_rd = bracket_set_1[[paste0("rd",rd,"_n")]]
    curr2_rd = bracket_set_2[[paste0("rd",rd,"_n")]]
    curr_rd = c(curr1_rd, curr2_rd)
    bracket_set[[paste0("rd",rd,"_n")]] = curr_rd
    
    # if (bracket_set$probs) {
    #   curr1_probs_rd = bracket_set_1[[paste0("probs_rd",rd,"_n")]]
    #   curr2_probs_rd = bracket_set_2[[paste0("probs_rd",rd,"_n")]]
    #   curr_probs_rd = c(curr1_probs_rd, curr2_probs_rd)
    #   bracket_set[[paste0("probs_rd",rd,"_n")]] = curr_probs_rd
    # }
  }
  return(bracket_set)
}

# bracket_set_a = tmm_sample_n_brackets_p(m=m,n=1,p=0.75)
# bracket_set_b = tmm_sample_n_brackets_p(m=m,n=1,p=0.75)
# bracket_set_c = tmm_merge_brackets_p(bracket_set_a, bracket_set_b)

###########################
###  Score bracket sets ###
###########################

tmm_compute_max_score <- function(submitted_brackets, true_brackets, 
                                  scoring_method="ESPN", expected_score=F) {
  #####
  # submitted_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS submitted brackets
  # true_brackets coNUM_TRUE_BRACKETSaiNUM_SUBD_BRACKETS true brackets, for MoNUM_TRUE_BRACKETSe Carlo simulation
  # scoring_method in {"ESPN", "num_correct"} determines
  # the score f(x|tau) of a submitted bracket x relative to a true bracket tau
  #####
  if (submitted_brackets$R != true_brackets$R) stop("bracket scoring not supported if the R's are not the same")
  R = submitted_brackets$R
  NUM_TRUE_BRACKETS = true_brackets$n
  NUM_SUBD_BRACKETS = submitted_brackets$n
  scores_mat = matrix(nrow=NUM_TRUE_BRACKETS, ncol=NUM_SUBD_BRACKETS)
  colnames(scores_mat) = paste0("sb_",1:NUM_SUBD_BRACKETS)
  rownames(scores_mat) = paste0("tb_",1:NUM_TRUE_BRACKETS)
  
  for (tb in 1:NUM_TRUE_BRACKETS) {
    scores_tb = numeric(NUM_SUBD_BRACKETS)
    
    ### holds which bits are correct from previous rounds
    correct_bits = list() 
    m_1 = 2^(R-1)
    ### base case: in round 0 all bits are "correct"
    correct_bits[[paste0("rd",0,"_n_","left")]] = rep(1, m_1*NUM_SUBD_BRACKETS)
    correct_bits[[paste0("rd",0,"_n_","right")]] = rep(1, m_1*NUM_SUBD_BRACKETS)
    
    for (rd in 1:R) {
      m_rd = 2^(R-rd)
      
      ### current round bits
      true_rd = true_brackets[[paste0("rd",rd,"_n")]] [(1:m_rd)+(tb-1)*m_rd]
      true_rd = rep(true_rd, NUM_SUBD_BRACKETS)
      subd_rd = submitted_brackets[[paste0("rd",rd,"_n")]]
      
      ### which bits are correct from previous round
      correct_bits_left_rd_prev = correct_bits[[paste0("rd",rd-1,"_n_","left")]]
      correct_bits_right_rd_prev = correct_bits[[paste0("rd",rd-1,"_n_","right")]]
      
      ### scores
      correct_bits_rd_left  = as.numeric(true_rd == subd_rd & true_rd == 0 & correct_bits_left_rd_prev)
      correct_bits_rd_right = as.numeric(true_rd == subd_rd & true_rd == 1 & correct_bits_right_rd_prev)
      correct_bits_rd = correct_bits_rd_left + correct_bits_rd_right
      
      ### update which bits are correct
      correct_bits[[paste0("rd",rd,"_n_","left")]] = correct_bits_rd[c(TRUE,FALSE)]
      correct_bits[[paste0("rd",rd,"_n_","right")]] = correct_bits_rd[c(FALSE,TRUE)]
      # browser()
      
      ### get scores for this round
      if (scoring_method == "ESPN") {
        scores_rd = correct_bits_rd * 2^(rd-1) * 10
      } else if (scoring_method == "Hamming") {
        scores_rd = correct_bits_rd
      } else {
        stop(paste0("scoring_method=", scoring_method," is not supported"))
      }
      scores_rd_mat = matrix(scores_rd, nrow = NUM_SUBD_BRACKETS, ncol = m_rd)
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

# ex_true_backets = tmm_sample_n_brackets_p(m,n=25,p=0.75)
# ex_submitted_backets = tmm_sample_n_brackets_p(m,n=500,p=0.75)
# ex_scores = tmm_compute_max_score(ex_submitted_backets, ex_true_backets, scoring_method = "Hamming")
# ex_scores
# ex_scoresE = tmm_compute_max_score(ex_submitted_backets, ex_true_backets, scoring_method = "Hamming", expected_score = T)
# ex_scoresE
