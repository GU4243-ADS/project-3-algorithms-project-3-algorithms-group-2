###################################################################
### Memory-based Collaborative Filtering Algorithm Starter Code ###
###################################################################

### Authors: CIndy Rush
### Project 3
### ADS Spring 2018


MS_data_transform <- function(MS) {
  
  ## Calculate UI matrix for Microsoft data
  ##
  ## input: data   - Microsoft data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(MS$V2[MS$V1 == "C"]))
  vroots <- sort(unique(MS$V2[MS$V1 == "V"]))
  
  nu <- length(users)
  nv <- length(vroots)
  
  # Initiate the UI matrix
  UI            <- matrix(0, nrow = nu, ncol = nv)
  row.names(UI) <- users
  colnames(UI)  <- vroots
  
  user_locs <- which(MS$V1 == "C")
  
  # Cycle through the users and place 1's for the visited vroots.
  for (i in 1:nu) {
    name     <- MS$V2[user_locs[i]]
    this_row <- which(row.names(UI) == name)
    
    # Find the vroots
    if (i == nu) {
      v_names <- MS$V2[(user_locs[i] + 1):nrow(MS)]
    } else {
      v_names <- MS$V2[(user_locs[i] + 1):(user_locs[i+1] - 1)]
    }  
    
    # Place the 1's
    UI[this_row, colnames(UI) %in% v_names] <- 1
  }
  return(UI)
}



movie_data_transform <- function(movie) {
  
  ## Calculate UI matrix for eachmovie data
  ##
  ## input: data   - movie data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(movie$User))
  movies <- sort(unique(movie$Movie))
  
  # Initiate the UI matrix
  UI            <- matrix(NA, nrow = length(users), ncol = length(movies))
  row.names(UI) <- users
  colnames(UI)  <- movies
  
  # We cycle through the users, finding the user's movies and ratings
  for (i in 1:length(users)) {
    user    <- users[i]
    movies  <- movie$Movie[movie$User == user]
    ratings <- movie$Score[movie$User == user]
    
    ord     <- order(movies)
    movies  <- movies[ord]
    ratings <- ratings[ord]
    
    # Note that this relies on the fact that things are ordered
    UI[i, colnames(UI) %in% movies] <- ratings
  }
  return(UI)
}  



calc_weight <- function(data, run.pearson=F, run.entropy=F, run.spearman=F, run.sqdiff=F, run.cosin = F, run.sim) {
  
  ## Calculate similarity weight matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        method - 'pearson'
  ##
  ## output: similarity weight matrix
  
  
  # Iniate the similarity weight matrix
  data       <- as.matrix(data)
  weight_mat <- diag(x = 1, nrow(data), nrow(data))
  weight_func <- function(rowA, rowB) {
    
    # weight_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    if (sum(joint_values) == 0) {
      return(0)
    } else {
      if (run.pearson) {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
      }
      if (run.entropy) {
          library("infotheo")
        return(mutinformation(rowA[joint_values], rowB[joint_values], method = 'emp'))
      }
      if (run.spearman) {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'spearman'))
      }
      if (run.sqdiff) {
        return(mean((rowA[joint_values]-rowB[joint_values])^2))
      }
      if(run.cosin){
        if(!require("lsa")){
          install.packages("lsa")
        }
        library("lsa")
      stand_rowA <- as.vector(scale(rowA[joint_values]))
      stand_rowB <- as.vector(scale(rowB[joint_values]))               
      return(cosine(stand_rowA, stand_rowB))
      }
      if(run.sim){
        
        j <- !is.na(rowA) | !is.na(rowB)
        rowA <- rowA[j]
        rowB <- rowB[j]
        if(length(rowA) <= 1){
          score <- 0
        }else{
          # Convert to 1/0 vector
          rowA <- ifelse(is.na(rowA), 0, 1)
          rowB <- ifelse(is.na(rowB), 0, 1)
          
          # First construct the user and object score matrix
          s_user <- diag(x = 1, 2,2)
          s_obj <- diag(x = 1, length(rowA), length(rowB))
          p_user <- rowA %*% t(rowB)
          # Find |O(X)|,|O(Y)|
          len_x <- sum(rowA)
          len_y <- sum(rowB)
          # Iteratively update the scores of user and objects
          for(k in 1:(K+1)){
            # Calculate s_user
            s_user[1,2]=s_user[2,1] <- sum(s_obj * p_user)*C1/(len_x*len_y)
            
            # Now update s_obj
            for(row_i in 1:nrow(s_obj)){
              for(col_j in 1:ncol(s_obj)){
                if(row_i == col_j){
                  s_obj[row_i, col_j] <- 1
                }else{
                  I_a <- as.vector(c(rowA[[row_i]],rowB[[row_i]]))
                  I_b <- as.vector(c(rowA[[col_j]], rowB[[col_j]]))
                  p_obj <- I_a %*% t(I_b)
                  s_obj[row_i, col_j] <- sum(s_user * p_obj) * C2/(sum(I_a) * sum(I_b))
                }
              }
            }
          }
          score <- s_user[1,2]
          
        }
        return(score)
      }
    }
  }
  
  # Loops over the rows and calculate sall similarities using weight_func
  ################ Note ##############
  # Since similarity weights are symmetric, I decided to only fill the upper
  # triangle in order to save computation time and space.
  for(i in 1:(nrow(data)-1)) {
    weight_mat[i, i:nrow(data) ] <- apply(data[i:nrow(data),], 1, weight_func, data[i, ])
    print(i)
  }
  weight_mat <- as.matrix(Matrix::forceSymmetric(round(weight_mat,4), uplo = "U"))
  return(weight_mat)
}






pred_matrix <- function(data, simweights) {
  
  ## Calculate prediction matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        simweights - a matrix of similarity weights
  ##
  ## output: prediction matrix
  
  # Initiate the prediction matrix.
  pred_mat <- data
  
  # Change MS entries from 0 to NA
  pred_mat[pred_mat == 0] <- NA
  
  row_avgs <- apply(data, 1, mean, na.rm = TRUE)
  
  for(i in 1:nrow(data)) {
    
    # Find columns we need to predict for user i and sim weights for user i
    cols_to_predict <- which(is.na(pred_mat[i, ]))
    num_cols        <- length(cols_to_predict)
    neighb_weights  <- simweights[i, ]
    
    # Transform the UI matrix into a deviation matrix since we want to calculate
    # weighted averages of the deviations
    dev_mat     <- data - matrix(rep(row_avgs, ncol(data)), ncol = ncol(data))
    weight_mat  <- matrix(rep(neighb_weights, ncol(data)), ncol = ncol(data))
    
    weight_sub <- weight_mat[, cols_to_predict]
    dev_sub    <- dev_mat[ ,cols_to_predict]
    
    pred_mat[i, cols_to_predict] <- row_avgs[i] +  apply(dev_sub * weight_sub, 2, sum, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
    print(i)
  }
  
  return(pred_mat)
}

