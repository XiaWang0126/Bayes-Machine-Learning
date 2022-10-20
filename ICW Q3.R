### Question 3 ----
library(caret)
library(pROC)
# Load the dataset
df=read.csv("newthyroid.txt",header = TRUE)

# Check if there is missing values
sum(is.na(df))

# Convert the label into factor
df$class=factor(df$class)

## Section 1 ----
set.seed(888)
# Create empty vectors to record the AUC scores aftwerwards
knn_list = rep(0,10)
lda_list = rep(0,10)

# For loop ten times the prediction process
# 1. We are going to random split the dataset once, 
# 2. Implement knn on training and evaluate the model with auc score
# 3. Add this auc to the knn_list
# 4. Repeat steps 2 and 3 for lda method (add to lda_list)
# 5. Repeat all the steps 1-4 again, 10 times.
for (i in 1:10){
  # Split the dataset into train and test
  train.index=createDataPartition(df$class,p=0.7,list=FALSE)
  train.feature=df[train.index,-1] # training features
  train.label=df$class[train.index] # training labels
  test.feature=df[-train.index,-1] # test features
  test.label=df$class[-train.index] # test labels

  # Scale the feature variables before kNN
  train.feature =scale(train.feature)
  test.feature = scale(test.feature)

############# kNN method #############
  # set up train control
  fitControl <- trainControl(## 5-fold CV
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    classProbs = TRUE,summaryFunction = twoClassSummary)
  # training process
  knn.Grid = expand.grid(k=c(3,5,7,9,11,15))
  knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "ROC",
             preProcess = c("center","scale"),
             tuneGrid=knn.Grid)
  knnFit
  plot(knnFit)
  knnFit$finalModel # the final model

  # ROC of knn on test set
  pred=predict(knnFit,test.feature)
  confusionMatrix(pred,test.label)
  knn.probs = predict(knnFit,test.feature,type="prob")
  knn.ROC = roc(predictor=knn.probs$h,
             response=test.label) # the ROC
  # Append the auc score of each iteration to the knn_list
  knn_list[i]=knn.ROC$auc 

####################LDA#####################
  # set up train control
  fitControl <- trainControl(## 5-fold CV
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    classProbs = TRUE,summaryFunction = twoClassSummary)

  # training the model
  ldaFit=train(train.feature,train.label, method = "lda",
             trControl = fitControl,
             metric = "ROC",
             preProcess = c("center","scale"))
             #tuneGrid=lda.Grid)
  ldaFit
  ldaFit$finalModel # the final model

  # ROC of knn on test set
  pred=predict(ldaFit,test.feature)
  confusionMatrix(pred,test.label)
  lda.probs = predict(ldaFit,test.feature,type="prob")
  lda.ROC = roc(predictor=lda.probs$h,
              response=test.label) 
  # Append the auc score of each iteration to lda_list
  lda_list[i]= lda.ROC$auc
}

## Section 2 ----
knn <- data.frame(knn_list)
lda <- data.frame(lda_list)
boxplot(knn$knn_list, lda$lda_list, col=c('orange', 'cornflowerblue'),main=" AUC Boxplot",
        names=c('knn', 'lda'), ylab='AUC score')






