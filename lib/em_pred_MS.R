em_pred_movie <- function(gamma, assign_mat) {

  #######PREDICTION - MS Data##########
  
  # exp_clust <- matrix(0, nrow = n_user, ncol = C)
  exp_score_mat <- matrix(0, nrow = n_user, ncol = n_movie)
  
  #This tells us what cluster each user belongs to
  for (n in 1:n_user) {
    max_clust <- which.is.max(assign_mat_samp[n,]) #This gives the most likely cluster for that particular user
    # exp_clust[n, max_clust] <- 1
    
    for (m in 1:n_movie) {
      score_prob <- max(gamma[,max_clust,m])
      exp_score_mat[n,m] <- score_prob
    }
  }
  return(exp_score_mat)
}
