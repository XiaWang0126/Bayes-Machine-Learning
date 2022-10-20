### Question 1 ----

# Import the libraries
library(tree)
library(randomForest)
library(gbm)
library(caret)
library(pROC)

# Import dataset 
data("GermanCredit")

# Data preprocessing
##Check the na values of the dataset
sum(is.na(GermanCredit))

## Delete the rows where the values are the same for all the observations
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] = list(NULL)

## Check the proportion of each class
bad <-subset(GermanCredit,GermanCredit$Class %in% c("Bad"))
good <-subset(GermanCredit,GermanCredit$Class %in% c("Good"))

## Convert the label into factors
GermanCredit$Class=factor(GermanCredit$Class)

# Split the data into training (70%) and test (30%) set 
set.seed(688)
train.index=createDataPartition(GermanCredit$Class,p=0.7,list=FALSE)
train.feature=GermanCredit[train.index,-10] # training features
train.label=GermanCredit$Class[train.index] # training labels
test.feature=GermanCredit[-train.index,-10] # test features
test.label=GermanCredit$Class[-train.index] # test labels

### Section 1 ----
############# Decision Tree ################
# Using caret package to build the tree
# 5-fold cross-validation process, repeated 1 time to obtain the optimal cp
fitcontrol=trainControl(method = "repeatedcv", number = 5,
                        repeats = 1)
set.seed(888) 
# Train the model with the tuning parameters
dt.rpart=train(train.feature,train.label, 
                  method = "rpart", tuneLength=5,
                  trControl = fitcontrol)
dt.rpart
dt.rpart$finalModel # The final model
# Plot the decision tree built
library(rattle)
fancyRpartPlot(dt.rpart$finalModel, sub=NULL)

# Predict on test
pred.dt=predict(dt.rpart,newdata=test.feature)

# The test error rate 
mean(pred.dt!=test.label)

### Section 2 ----
################## Random Forest ##################
# Using caret package for Random Forest
fitControl=trainControl( method = "repeatedcv", number = 5,
                         repeats = 1)
set.seed(888) 
# Train the model with the tuning parameters
rfFit=train(train.feature,train.label,method="rf",metric="Accuracy",
            trControl=fitControl,tuneLength=5, ntree=1000)
rfFit
plot(rfFit)
rfFit$finalModel # The final model

# Plot the variable importance
import.rf <- varImp(rfFit, scale = FALSE)$importance
new.df <- cbind(Features = rownames(import.rf), import.rf)
import.sort <- new.df[order(new.df$Overall),]
par(mar=c(4.5,10.7,3,3)) # set margins to the plot

# Plot format: barplot
barplot(
  height = sort(import.sort$Overall[50:59]),
  names.arg = import.sort$Features[50:59],
  #col = rgb(0.2, 0.7, 0.8, 0.5),
  #col = "#3366CC",
  col = "#6699FF",
  horiz = T,
  las = 1,
  xlab = "Mean Decrease Gini",
  border = F,
  cex.names = 0.8,
  cex.axis = 1,
  main = 'Random Forest Variable Importance',
  xlim=range(pretty(c(0, import.sort$Overall)))
)

# Predict on test
pred.rf=predict(rfFit,newdata=test.feature)
# The test error rate 
mean(pred.rf!=test.label)


### Section 3 ----
## Decision Tree ROC
confusionMatrix(pred.dt,test.label)
# Prediction probability
dt.probs = predict(dt.rpart,test.feature,type="prob")
dt.ROC = roc(predictor=dt.probs$Bad,
             response=test.label)
dt.ROC$auc

## Random forest ROC
confusionMatrix(pred.rf,test.label)
# Prediction probability
rf.probs = predict(rfFit,test.feature,type="prob")
rf.ROC = roc(predictor=rf.probs$Bad,
              response=test.label)
rf.ROC$auc

# Plot the ROC curve of DT and RF
plot(dt.ROC,col='orange',main=" ROC curve")
plot(rf.ROC, col='cornflowerblue' ,add=TRUE)
# Add legend to the plot
legend( x="bottomright", legend=c("Decision Tree","Random Forest"),
        lty = c(1,1), col = c('orange', 'cornflowerblue'), lwd =2 )







