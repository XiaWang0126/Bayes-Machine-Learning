###############################################################################
#### This function provides training indexes for K-fold cross-validation:
#### Input: label: label vector of the dataset
####        K: number of k-fold
####        seed: random state

#### Output: a list with the training indexes for each iteration of K-fold
###############################################################################
########## Define function
training.indexes=function(label, K, seed){
  # The total number of observations per fold
  # Round down in case that it is not a integer the output of the division
  num <- floor(length(label)/K)
  # Get the number of classes of the dataset and convert into dataframe
  label_df = data.frame(label)
  label_df$label = factor(label_df$label)
  # Convert the index of each observation into new column 
  label_df <- cbind(Index = rownames(label_df), label_df)
  # Split the dataframe by each class
  label_split <- split(label_df, label_df$label)
  
  # Create an empty dataframe to record the index of each iteration
  sample_index <- data.frame()
  # for loop the iteration of folds
  for (i in 1:length(unique(label))){ # the number of classes 
    # Get the index of each class
    index = label_split[[i]][["Index"]]
    # Know the proportion of each class in each fold according to the original proportion
    proportion <- floor(length(index)/K) # Round down in case it is not a integer
    # Sample each class and put it into a matrix
    index_matrix <- matrix(sample(as.integer(index), size=proportion*K, replace=FALSE),ncol=K)
    # Append to the dataframe the indexes of the matrix
    sample_index <- rbind(sample_index, index_matrix)
  }
  
  # Check there is no duplicated indexes
  duplicated(t(sample_index))
  
  # The following lines will create a list with the training indexes of each 
  # iteration of the K-fold
  sample_list <- as.list(sample_index) # change of dataframe into list
  unlst <- unname(unlist(sample_list)) # all the indexes sampled in the folds
  
  # Empty list to record the indexes of each iteration of the K-fold
  index_list = list()
  # Empty vector to append the training indexes that are different from validation indexes
  vec = rep(0, length(unlst)-num)
  for (i in 1:K){
    valid.index <- sample_list[[i]] # Get the indexes of validation fold in each iteration
    vec <- setdiff(unlst,valid.index) # Append the indexes that do not appear in the validation
    index_list[[i]] <- vec # Append the vector to the empty list created
  }
  
  # In case that the number of K-fold is not a divisor of the number of observations, 
  # with the observations that have not been selected, we will append them into the 
  # folds sequentially. Therefore, some folds might have 1 observation of difference
  index_label <- as.integer(label_df$Index) # record the list of total number of the indexes in the dataset
  # if the total number of indexes in the dataset and the indexes used in all the folds are NOT the same
  if (length(index_label) != length(unlst)){ 
    diff <- setdiff(index_label,unlst) # Get the indexes that have not been sampled into a vector
    # Append the elements of the vector to the index list sequentially
    for (i in 1:length(diff)){ 
      index_list[[i]] <- c(index_list[[i]],diff[i])
    }
  }
  
  return(index_list)
}

###############################################################################
############ Implement the define function to GermanCredit dataset
library(caret)
data("GermanCredit")
label= GermanCredit$Class

# Implement function defined above
cv = training.indexes(label, K=7, seed=126)
cv

