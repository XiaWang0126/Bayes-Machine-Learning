# Read Table 
cp <- read.csv("customer-personality.csv")

################### Part 1: Data Description ###############################

# Data pre-processing
# Step 1: Check if same value of each rows within columns
# By using n_distinct from dplyr
require(dplyr)
sapply(cp, n_distinct)

# Step 2: Check missing value and drop NA values
# Check missing value
sum((is.na(cp))) 
which(colSums(is.na(cp))>0)
sum(is.na(cp$Income)) 

# Drop NA values
cp <- na.omit(cp)

# Step 3: Feature engineering

# 1. Category married status:
# Change single, yolo, divorced, alone, absurd into single category
cp$Marital_Status[which(cp$Marital_Status %in% c("YOLO", "Divorced", "Alone", "Absurd"))]<-"Single"
cp$Education[which(cp$Education%in% c("2n Cycle"))]<-"Master"


# 2. Calculate the age of the customers
cp$Year_Birth <- 2022 - cp$Year_Birth

# 3. Covert Dt_customer into the enrollment days
cp$Dt_Customer = as.Date(cp$Dt_Customer,'%d-%m-%Y')
cp$Dt_Customer <- as.integer(difftime( "2022-04-01",cp$Dt_Customer, units = "days"))

# Step 4: Check if there is collinearity problem of the dataset
# 1. Plot the correlation of each pair of variables
library(corrplot)
library(RColorBrewer)
corr <- cor(cp[,unlist(lapply(cp,is.numeric))][,-1])
corrplot(corr,type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"),method = 'number',  
         number.cex = 0.8, number.font =2, tl.col = 'black', 
         tl.cex = 0.7, main ='The correlation of each pair of variables',
         mar = c(3, 3, 3, 3))
# There is no high correlation between each pair of variable

# 2. Briefly examine the data
round(apply(cp[,unlist(lapply(cp,is.numeric))][,-1], 2, mean),2)
round(apply(cp[,unlist(lapply(cp,is.numeric))][,-1], 2, var),2)


# Step 5: Check the distribution of each variable 
# Plot histogram of each variable
library(Hmisc)
hist.data.frame(cp[,unlist(lapply(cp,is.numeric))][,-1])

# Step 6: Convert categorical variables into dummy variables
# Get dummies
library('fastDummies')
cp <- fastDummies::dummy_cols(cp, remove_first_dummy = FALSE)
cp <- cp[, -c(3,4)]


################### Part 2 Data Analysis - Customer Profile #####################
# Customer profile
# Step 1: 
# Select the variables related to demographic information of the customer profile
cp.profile <- cp[, c(2:5,19:26)]

# Step 2: Scale the original dataset
set.seed(888)
cp.profile.scale <- data.frame(scale(cp.profile))

# Step 3: Plot the hierarchical clustering dendrogram of customer profiles
clusters <- hclust(dist(cp.profile.scale), method="ward.D2")
plot(clusters,labels=FALSE)
clusterCut <- cutree(clusters, 7)
# Assign the cluster label of each observation to the original data
cp.profile$Profile <- clusterCut 
cp$Profile <- clusterCut 

# Step 4: Get more insights of the each type of customer profile

# 1. Split the subset according to the label of customer profile
cluster.profile <- split(cp.profile, f= cp.profile$Profile)
colnames = c(colnames(cluster.profile[[1]][,-c(3:4, 13)]))
mean_df <-data.frame()

# 2. Get the mean value of each variable for each customer profile subset
for (i in 1:length(cluster.profile)){
  vec <- round(apply(cluster.profile[[i]][,-c(3:4, 13)],2,mean),2)
  mean_df <- rbind(mean_df,vec)
}
names(mean_df) <- colnames

# 3. Plotting the heatmap of the customer profile

heatmap(as.matrix(mean_df), Colv = NA, Rowv=NA, scale='column', 
        margins = c(30,3), cexRow = 1, cexCol = 0.5)



################### Part 3: Independent Component Analysis - Customer Behaviour ###########

# Step 1. The Shapiro-Wilk test to continuous variable of the original dataset 
shapiro.test.df <- cp[,c(6:18)]
for (i in 1:length(shapiro.test.df)){
  print(shapiro.test(shapiro.test.df[,i]))
}

# Step 2. Apply ICA to find the latent sources of the original data

# 1. Dimension Reduction Processing
# Scale the original data to 0-1
std_function<- function(x){
  for(i in seq(length(x))){
    x[,i] <- (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
  }
  return(x)
}
cp.segment <- cp[,6:18]
cp.segment <- std_function(cp.segment[,1:13])

# 2. Apply PCA to select the optimal number of components
cp.pca = prcomp(cp.segment, scale = FALSE)
cp.pca$sdev # standard deviation
cp.var = cp.pca$sdev ^2

# To choose the number of principal component, need to calculate the variance explained
cp.explain <- cp.var/sum(cp.var) # variance / sum of variance
cp.explain
sum(cp.explain[1:4]) 
# the first four principal components could explain 77.79% of total variance

plot(cp.explain, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(cp.explain), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")

# 3. Apply ICA to obtain the latent sources
library(ica)
ica.cp <- icafast(cp.segment, 4, center = TRUE, maxit = 200, tol = 1e-6,
                  Rmat = diag(4), alg = c("par", "def"),
                  fun = c("logcosh"), alpha = 1)

# Get the loading of each latent source by using the mixing matrix
loading.ica<-data.frame(scale(ica.cp[['M']]))
# To see the variance explained by each latent variable
var <- ica.cp$vafs
var # IC1-IC4: 0.3125420 0.1792414 0.1755805 0.1105057
# accumulated variance explained
sum(var)  #[1] 0.7778696
rownames(loading.ica) <- colnames(cp[,6:18])
colnames(loading.ica) <- c('IC1','IC2','IC3','IC4')

# 4. The Shapiro-Wilk test to each latent variable (IC)
cp.behaviour <- data.frame(ica.cp$S[,1:4]) # S: Matrix of source signal estimates

for (i in 1:length(cp.behaviour)){
  print(shapiro.test(cp.behaviour[,i]))
}

# 5. Plotting independent component combination of X variables
# S combination of X variables
require('ggrepel')
p1 <- ggplot(loading.ica, aes(IC1, IC2))
p1 + geom_label_repel(aes(label = rownames(loading.ica)), size = 4, max.overlaps = Inf) +
  geom_point(colour = 'blue',size = 1) +
  theme(aspect.ratio = 0.2, 
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

p2 <- ggplot(loading.ica, aes(IC3, IC4))
p2 + geom_label_repel(aes(label = rownames(loading.ica)), size = 3, max.overlaps = Inf) +
  geom_point(colour = 'blue',size = 1) +
  theme(aspect.ratio = 0.2,
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


# Step 3: Customer purchasing behaviour clustering
# 1. Scale the original data to 0-1 
cp.behaviour <- std_function(cp.behaviour[,1:4])
colnames(cp.behaviour) <- c('IC1','IC2','IC3','IC4')

# 2. K-means clustering to observe the optimal number of cluster
library('factoextra')

# Elbow method 
fviz_nbclust(cp.behaviour, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle


# 3. Plot the hierarchical clustering dendrogram of customer segments
cp.dis <- dist(cp.behaviour, method = "euclidean")
cp.hc <- hclust(cp.dis, method = "ward.D2")
plot(cp.hc, labels = FALSE, main = "Ward", xlab = "")

# Extract 4 clusters
segmentcut <- cutree(cp.hc, 4)

# Add segment labels of each observation to the dataset
cp$Segment <-segmentcut
cp.behaviour$Segment <- segmentcut


# Step 4: Visualing the segments clusters of different customer purchasing behaviour
# 1. Heatmap of segment clusters
library(dplyr)
library('reshape')
# Calculate the mean of each IC in each segment clusters
cp.group <- data.frame(cp.behaviour %>% group_by(Segment)  %>%
  summarise(IC1 = mean(IC1),
            IC2 = mean(IC2),
            IC3 = mean(IC3),
            IC4 = mean(IC4),
            .groups = 'drop'))

library(RColorBrewer)
# Transform the matrix in long format
cp.melt <- melt(cp.group, id.vars = 'Segment', 
                measure.vars = c('IC1', 'IC2', 'IC3','IC4'),
                variable_name = 'Latent_Variable',
                value.name = 'value')

library(ggplot2)
# Plotting the heatmap of segments

ggplot(cp.melt, aes(x=Latent_Variable, y=Segment, fill = value)) + 
  geom_tile(color = "white",lwd = 1.5, linetype = 1) +
  coord_fixed() +
  geom_text(aes(label = round(value,2)), color = "white", size = 2)+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 5, title = 'Value')) +
  theme(aspect.ratio = 0.2)


# 2. Snake plot of the segments
p3 <- ggplot(cp.melt, aes(x= Latent_Variable, y=value)) + 
  geom_line(aes(group = factor(Segment), color=factor(Segment)), size = 1.2) + 
  geom_point(aes(color=factor(Segment)), size = 3) +
  theme(plot.title = element_text(size = 16)) +
  labs(element_text(size=15)) +
  labs(x ="Latent Variable", y = "Value",color = "Segments") +
  theme_minimal()
p3 + theme(legend.position="right", aspect.ratio = 0.2)


################### Part 4: Findings in ICA #########################

# Step 1: Scatter plot of each segment with combination of different ICs
# 1. Get the score of each independent component 
scatter.ica<-data.frame(scale(ica.cp[['S']]))
scatter.ica$Segment <- as.character(cp$Segment)
scatter.ica$Profile <- as.character(cp$Profile)


library('ggpubr')
# 2. Plot the scatter plots
ggscatter(
  scatter.ica, x = 'X1', y = 'X3', 
  color = 'Segment', palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  size = 1,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", round(var[1],4)*100, "% )" ),
  ylab = paste0("Dim 3 (", round(var[3],4)*100, "% )" )
) +
  stat_mean(aes(color = Segment), size = 4)


ggscatter(
  scatter.ica, x = 'X2', y = 'X4', 
  color = 'Segment', palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  size = 1,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 2 (", round(var[2],4)*100, "% )" ),
  ylab = paste0("Dim 4 (", round(var[4],4)*100, "% )" )
) +
  stat_mean(aes(color = Segment), size = 4)


# Step 2: Get more insights within each segment combining with customer profile
# 1. Split by customer behaviour segments into different subset
profile.segment <- split(cp, f= cp$Segment)

# 2. Plot the barchart of each segment
library(DataCombine)
par(mfrow = c(2,2))
# par(margin(3,1,1,1))
for (i in 1:length(profile.segment)){
  seg <- data.frame(profile.segment[[i]][["Profile"]])
  colnames(seg) <- 'Profile'
  profile<- data.frame(table(seg))
  
  if (i==3) {
    profile$seg <- as.character(profile$seg)
    new_row <- c('5', 0)
    profile <- InsertRow(profile, new_row, RowNum = 5)
    profile$Freq <- as.numeric(profile$Freq)
  }
  
  
  my_bar <- barplot(profile$Freq, names.arg=profile$seg, 
                    col=c(rgb(0.2, 0.7, 0.8, 0.5)),
                  ylim=range(pretty(c(0, 200))), 
                  xlab = 'Customer profile',
                  ylab= 'Number of Customer',
                  cex.lab = 1)
  text(x = my_bar, y = profile$Freq, label = profile$Freq, 
       pos = 3, cex = 0.8, col = "black")
  title(main = paste('Customer Segment',i), font = 2, cex.main=1.2)
}



# Step 3: Segment statistical description
cluster.segment <- split(cp, f= cp$Segment)
colnames = c(colnames(cluster.segment[[1]][,-c(1, 27:28)]))
mean_segment <-data.frame()

# Calculate the mean of each variable in each segment
for (i in 1:length(cluster.segment)){
  vec <- round(apply(cluster.segment[[i]][,-c(1, 27:28)],2,mean),2)
  mean_segment <- rbind(mean_segment,vec)
}
names(mean_segment) <- colnames
mean_segment<- t(rbind(mean_segment,round(apply(cp[,-c(1,27:28)],2,mean),2)))
colnames(mean_segment) <- c('Segment1','Segment2','Segment3','Segment4','Population')
print(mean_segment)

# Calculate the ratios of between the average for each cluster 
# and the average of the population and subtract 1
ratio_segment <- data.frame(mean_segment[,1:4]/mean_segment[,5]-1)
print(ratio_segment)



