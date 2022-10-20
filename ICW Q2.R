### Question 2 ----
library(caret)
set.seed(126)
# Generate random dataset with 2 variables and 3 classes
# Firstly, generate random values for each variable of each class
# Create dataframe for each class
# Merge dataframe of each class together as 1
var1 <- sample(x = 1:25, size = 50, replace = TRUE)
var2 <- runif(n = 50, min = 0, max = 1.75)
df_1 <- data.frame(var1, var2)
df_1$Class <- "Cod"
var1 <- sample(x = 20:35, size = 50, replace = TRUE)
var2 <- runif(n = 50, min = 0, max = 1.5)
df_2 <- data.frame(var1, var2)
df_2$Class <- "Salmon"
var1 <- sample(x = 15:40, size = 50, replace = TRUE)
var2 <- runif(n = 50, min = 0.75, max = 2)
df_3 <- data.frame(var1, var2)
df_3$Class <- "Tuna"
df <- rbind(df_1, df_2,df_3)
df$Class=factor(df$Class)

### Section 1 ----
# Scatter plot of the dataset
# Settings of the plot in terms of color and symbols of each class
cols <- c("cornflowerblue", "orange","purple")
pchs <- c(1, 2, 4)
par(mar=c(4,4,2,2))
plot(df$var1, df$var2, 
     col= cols[df$Class],
     pch = pchs[df$Class], 
     xlab="Variable 1",
     ylab= "Variable 2")
legend( x="topleft", legend=c("Cod","Salmon","Tuna"), 
        pch = pchs, col = cols, bg=NULL, bty="n")

### Section 2 ----
# Split the dataset into train (50%) and test (50%) set 
set.seed(126)
train.index=createDataPartition(df$Class,p=0.5,list=FALSE)
train.feature=df[train.index,-3] # training features
train.label=df$Class[train.index] # training labels
test.feature=df[-train.index,-3] # test features
test.label=df$Class[-train.index] # test labels

## Linear kernel svm ----
# Using caret package
# Train the model without tuning
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Linear=train(train.feature,train.label, method = "svmLinear",
                 trControl=fitControl,
                 preProcess = c("center", "scale"))
svm.Linear
# By default, the package holds parameter C at 1, with accuracy 0.69
# Train the model with tuned parameter C
grid_linear=expand.grid(C = c(0.1,0.5,1,1.5,2,5,10,15,20,30))
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Linear2=train(train.feature, train.label, method = "svmLinear",
                  trControl=fitControl,
                  preProcess = c("center", "scale"),
                  tuneGrid = grid_linear)
svm.Linear2
plot(svm.Linear2, main="Linear Kernel")
# The optimal model with highest accuracy: 0.72, C=10

# Test on the test
# Without tuning model
pred_linear=predict(svm.Linear,newdata=test.feature)
mean(pred_linear==test.label)
# Accuracy: 0.64

#With tuning model
pred_linear2=predict(svm.Linear2,newdata=test.feature)
mean(pred_linear2==test.label)
# Accuracy:0.64

# Check accuracy of the best model:
table(pred_linear, test.label)


## Polynomial kernel ----
# Train the model
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Poly=train(train.feature,train.label, method = "svmPoly",
                 trControl=fitControl,
                 preProcess = c("center", "scale"))
svm.Poly
plot(svm.Poly, main='Default results')
# The model obtained by default is degree =3, scale=0.1, C=0.5, Accuracy:0.7066667

# Tune the parameters
# After some tries, the range of parameters are showed below
grid_poly=expand.grid(degree= seq(1,5, length=5),
                      scale= seq(0.1, 0.5, length=5),
                      C = c(0.1, 0.5, 1, 5, 10))

fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Poly2=train(train.feature,train.label, method = "svmPoly",
               trControl=fitControl,
               preProcess = c("center", "scale"),
               tuneGrid = grid_poly)
svm.Poly2
plot(svm.Poly2, main="Polynomial Kernel")
# The model with highest Accuracy: 0.76
# degree= 5, scale=0.4, C=0.1

# Test on default model
pred_poly=predict(svm.Poly,newdata=test.feature)
mean(pred_poly==test.label)
# Accuracy on test: 0.613

# Test on self-tuned model
pred_poly2=predict(svm.Poly2,newdata=test.feature)
mean(pred_poly2==test.label)
# Accuracy on test: 0.68

# Check accuracy of the best model:
table(pred_poly2, test.label)



## RBF kernel ----
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Radial=train(train.feature,train.label, method = "svmRadial",
               trControl=fitControl,
               preProcess = c("center", "scale"),
               tuneLength = 5)
svm.Radial
plot(svm.Radial, main= 'Default Radial Kernel')
# The model obtained is with Accuracy: 0.6933333, gamma=1.39858, C=2

# Tune the parameters
grid_radial=expand.grid(sigma = c(0.5, 1.3, 2, 5, 10),
                        C = c(0.1, 0.5, 1.5, 2, 5))

fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)
set.seed(126)
svm.Radial2=train(train.feature,train.label, method = "svmRadial",
                trControl=fitControl,
                preProcess = c("center", "scale"),
                tuneGrid = grid_radial)
svm.Radial2
plot(svm.Radial2, main= "RBF Kernel")
# The optimal model is with highest Accuracy: 0.77, gamma= 5, C=5


# Test on default model
pred_radial=predict(svm.Radial,newdata=test.feature)
mean(pred_radial==test.label)
# Accuracy: 0.626

# Test on self-tuned model
pred_radial2=predict(svm.Radial2,newdata=test.feature)
mean(pred_radial2==test.label)
# Accuracy: 0.533

# Check accuracy of the best model:
table(pred_radial, test.label)

