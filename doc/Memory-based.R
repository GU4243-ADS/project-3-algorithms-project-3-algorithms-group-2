
###################################################################
### Memory-based Collaborative Filtering Algorithm Starter Code ###
###################################################################

### Authors: 
### Project 3
### ADS Spring 2018


########################################################
######## Building the UI matrix for the MS Data ########
########################################################

# install.packages("DescTools")
library("DescTools")
# install.packages("infotheo")
library("infotheo")


setwd("/Users/wcheng/Desktop/Spring 2018/data science/project-3-algorithms-project-3-algorithms-group-2")
source("./lib/functions.R")

dir_MS <- "/data/Proj3_Data/MS_sample/"


# Load the data
MS_train <- read.csv(paste(getwd(),dir_MS, "data_train.csv", sep = ""), as.is = TRUE, header = TRUE)
MS_train <- MS_train[, 2:4]


# Transform from narrow to wide, i.e. user-item matrix 
# using MS_data_transform function

# Below takes 2.17 minutes
MS_UI <- MS_data_transform(MS_train)
save(MS_UI, file = "./output/MS_UI.RData")

###############################################################
#################### Constants definition #####################
###############################################################

run.pearson <- F
run.entropy <- F
run.spearman <- F
run.sqdiff <- F
run.cosin <- F




###############################################################
######## Building the UI matrix for the EachMovie Data ########
###############################################################

dir_mov <- "/data/Proj3_Data/eachmovie_sample/"

# Load the data
movie_train <- read.csv(paste(getwd(),dir_mov, "data_train.csv", sep = ""), as.is = TRUE, header = TRUE)
movie_train <- movie_train[, 2:4]

# # How we might fill in the user-item matrix using %in%
# 
# # Find sorted lists of users and vroots
# users  <- sort(unique(movie_train$User))
# movies <- sort(unique(movie_train$Movie))
# 
# # Initiate the UI matrix
# UI            <- matrix(NA, nrow = length(users), ncol = length(movies))
# row.names(UI) <- users
# colnames(UI)  <- movies
# 
# # We consider just user 1, finding user 1's movies and ratings
# movies  <- movie_train$Movie[movie_train$User == users[1]]
# ratings <- movie_train$Score[movie_train$User == users[1]]
# 
# ord     <- order(movies)
# movies  <- movies[ord]
# ratings <- ratings[ord]
# 
# system.time(UI[1, colnames(UI) %in% movies] <- ratings)
# 
# # How we might fill in the user-item matrix using loops 
# 
# long.in <- function(movies, ratings) {
#   
#   # Cycle through the ratings, find the corresponding column
#   for (i in 1:length(ratings)) {
#     column <- which(colnames(UI) == movies[i])
#     UI[2, column] <- ratings[i]
#     print(column)
#   }
#   
# }
# 
# 
# system.time(long.in(movies, ratings))
# 
# all(UI[1, ] == UI[2,], na.rm = TRUE)

# Compute the full matrix
# Below takes about 4 minutes

movie_UI <- movie_data_transform(movie_train)
save(movie_UI, file = "./output/movie_UI.RData")

# Some calculations
total_ratings <- rowSums(movie_UI, na.rm = TRUE)

table(total_ratings)
mean(total_ratings)
median(total_ratings)


# If we want to directly pull out the user-item matrix from the data folder
load("./output/MS_UI.Rdata")
load("./output/movie_UI.Rdata")


weight_func <- function(rowA, rowB) {
  
  # weight_func takes as input two rows (thought of as rows of the data matrix) and 
  # calculates the similarity between the two rows according to 'method'
  
  joint_values <- !is.na(rowA) & !is.na(rowB)
  if (sum(joint_values) == 0) {
    return(0)
  } else {
    if (method == 'pearson') {
      return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
    }
  }
}

# Loops over the rows and calculate sall similarities using weight_func
for(i in 1:nrow(data)) {
  weight_mat[i, ] <- apply(data, 1, weight_func, data[i, ])
  print(i)
}
return(round(weight_mat, 4))




# 




#################################################################
######## Calculating the Similarity Weights of the Users ########
#################################################################

# Initiate the similarity weight matrix

movie_UI         <- as.matrix(movie_UI)
movie_sim_weight <- matrix(NA, nrow = nrow(movie_UI), ncol = nrow(movie_UI))

# # Can calculate Pearson correlation between two rows of UI matrix as:
# 
# rowA <- movie_UI[1, ]
# rowB <- movie_UI[2, ]
# 
# cor(rowA, rowB, method = 'pearson', use = "pairwise.complete.obs")
# 
# # Another way:
# 
# joint_values <- !is.na(rowA) & !is.na(rowB)
# cor(rowA[joint_values], rowB[joint_values], method = 'pearson')
# 
# # First fill in row 1 of the similarity matrix using apply
# 
# system.time(vec1 <- apply(movie_UI, 1, cor, movie_UI[1, ], method = 'pearson', use = "pairwise.complete.obs"))
# 
# # Now fill in row 1 of the similarity matrix looping over the columns and 
# # calculating pairwise correlations
# 
# long.way <- function(row.num) {
#   
#   for(i in 1:nrow(movie_UI)) {
#     movie_sim_weight[row.num, i] <- cor(movie_UI[i, ], movie_UI[row.num, ], method = 'pearson', use = "pairwise.complete.obs")
#   }
#   
# }
# 
# system.time(long.way(1))



# Calculate the pearson weights on the movie data
# The below took 87 minutes on my Macbook, 35 on my iMac

movie_sim <- calc_weight(movie_UI, run.pearson = T)
save(movie_sim, file = "./output/movie_sim.RData")


# Calculate the pearson weights on the MS data
# The below took 30 minutes on my Macbook and 14 on my iMac

MS_sim <- calc_weight(MS_UI, run.pearson = T)
save(MS_sim, file = "./output/MS_sim.RData")


# Calculate the entropy weights on the movie data
# The below took  minutes on my Macbook,  on my iMac

tm_movie_ent <- system.time(movie_ent <- 
                              calc_weight(movie_UI, run.entropy = T))
save(movie_ent, file = "./output/movie_ent.RData")


# Calculate the entropy weights on the MS data
# The below took  minutes

tm_MS_ent <- system.time(MS_ent <- 
                           calc_weight(MS_UI, run.entropy = T))
save(MS_ent, file = "./output/MS_ent.RData")

# Calculate the spearman weights on the movie data
# The below took  minutes

tm_movie_spm <- system.time(movie_spm <- 
                              calc_weight(movie_UI,run.spearman = T))
save(movie_spm, file = "./output/movie_spm.RData")


# Calculate the spearman weights on the MS data
# The below took  minutes

tm_MS_spm <- system.time(MS_spm <- 
                           calc_weight(MS_UI, run.spearman = T))
save(MS_spm, file = "./output/MS_spm.RData")

# Calculate the cosin weights on the movie data
# The below took  minutes

tm_movie_cos <- system.time(movie_cos <- 
                              calc_weight(movie_UI,run.cosin = T))
save(movie_cos, file = "./output/movie_cos.RData")


# Calculate the cosin weights on the MS data
# The below took  minutes

tm_MS_cos <- system.time(MS_cos <- 
                           calc_weight(MS_UI, run.cosin = T))
save(MS_cos, file = "./output/MS_cos.RData")

# Calculate the squared difference weights on the movie data
# The below took  minutes

tm_movie_sqd <- system.time(movie_sqd <- 
                              calc_weight(movie_UI,run.sqdiff = T))
save(movie_sqd, file = "./output/movie_sqd.RData")


# Calculate the cosin weights on the MS data
# The below took  minutes

tm_MS_sqd <- system.time(MS_sqd <- 
                           calc_weight(MS_UI, run.sqdiff = T))
save(MS_sqd, file = "./output/MS_sqd.RData")



###########################################################
######## Calculating the Predictions for the Users ########
###########################################################



# # Calculate the predictions for user 1
# 
# # Initiate the prediction matrix and find the columns we need to predict
# # for user 1.
# pred_mat        <- MS_UI
# cols_to_predict <- which(MS_UI[1, ] == 0)
# num_cols        <- length(cols_to_predict)
# 
# # Transform the UI matrix into a deviation matrix since we want to calculate
# # weighted averages of the deviations
# neighb_weights <- MS_sim[1, ]
# row_avgs       <- apply(MS_UI, 1, mean, na.rm = TRUE)
# dev_mat        <- MS_UI - matrix(rep(row_avgs, ncol(MS_UI)), ncol = ncol(MS_UI))
# 
# # We'll calculate the predictions in two ways:
# 
# # First by looping over items where we want to make predictions
# 
# 
# for (i in 1:num_cols) {
#   
#   # For each column to predict, first find all deviations for that item
#   neighb_devs <- dev_mat[ ,cols_to_predict[i]]
#   
#   # For each column to predict, calculate the prediction as the weighted average
#   pred_mat[1, cols_to_predict[i]] <- row_avgs[1] +  sum(neighb_devs * neighb_weights, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
# }
# 
# 
# # Now using matrix equations
# 
# pred_mat2 <- MS_UI
# 
# weight_mat  <- matrix(rep(neighb_weights, ncol(MS_UI)), ncol = ncol(MS_UI))
# weight_sub  <- weight_mat[, cols_to_predict]
# neighb_devs <- dev_mat[ ,cols_to_predict]
# 
# # Now fill in all of row 1 with matrix equations
# pred_mat2[1, cols_to_predict] <- row_avgs[1] +  apply(neighb_devs * weight_sub, 2, sum, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
# 
# 
# # They're the same
# all(pred_mat2[1,] == pred_mat[1, ])


# Calculate predictions for MS
# This calculation took me 15 minutes

MS_pred <- pred_matrix(MS_UI, MS_sim)
save(MS_pred, file = "./output/MS_pred.RData")

# Calculate predictions for movies
# This calculation took me 1+ hour

movie_pred <- pred_matrix(movie_UI, movie_sim)
save(movie_pred, file = "./output/movie_pred.RData")
