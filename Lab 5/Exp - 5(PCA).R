#load data
data("USArrests")



#view first six rows of data
head(USArrests)



#calculate principal components
#Be sure to specify scale = TRUE so that each of the variables in the 
#dataset are scaled to have a mean of 0 and a standard deviation of 1 
#before calculating the principal components.
results <- prcomp(USArrests, scale = TRUE)
results
results$rotation
?prcomp



#that eigenvectors in R point in the negative direction by default, 
#so we'll multiply by -1 to reverse the signs.
#reverse the signs
results$rotation <- -1*results$rotation
#display principal components
results$rotation
#We can see that the first principal component (PC1) has high values 
#for Murder, Assault, and Rape which indicates that this principal 
#component describes the most variation in these variables.
#We can also see that the second principal component (PC2) has a high 
#value for UrbanPop, which indicates that this principle component 
#places most of its emphasis on urban population.



#the principal components scores for each state are stored in results$x. 
#We will also multiply these scores by -1 to reverse the signs:
results$x
#reverse the signs of the scores
results$x <- -1*results$x
#display the first six scores
head(results$x)



#plot PCA
biplot(results, scale = 0)
#From the plot we can see each of the 50 states represented in a simple 
#two-dimensional space.
#The states that are close to each other on the plot have similar 
#data patterns in regards to the variables in the original dataset.
#We can also see that the certain states are more highly associated 
#with certain crimes than others. For example, Georgia is the state 
#closest to the variable Murder in the plot.



#display states with highest murder rates in original dataset
head(USArrests[order(-USArrests$Murder),])



#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)
#From the results we can observe the following:
#The first principal component explains 62% of the total variance in the dataset.
#The second principal component explains 24.7% of the total variance in the dataset.
#The third principal component explains 8.9% of the total variance in the dataset.
#The fourth principal component explains 4.3% of the total variance in the dataset.
#Thus, the first two principal components explain a majority of the total 
#variance in the data.
#Thus, it's valid to look at patterns in the biplot to identify states 
#that are similar to each other.



#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)




#create scree plot
qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
#if the first two or three PCs have capture most of the information, 
#then we can ignore the rest without losing anything important. 
#A scree plot shows how much variation each PC captures from the data. 
#The y axis is eigenvalues, which essentially stand for the amount of 
#variation. Use a scree plot to select the principal components to keep.
#A scree plot always displays the eigenvalues in a downward curve, 
#ordering the eigenvalues from largest to smallest.
#An ideal curve should be steep, then bends at an "elbow" - this is your 
#cutting-off point - and after that flattens out.



biplot(results,choices=c(1,2))

#load data
install.packages("xlsx")
library("xlsx")
otter <- read.xlsx('F:/VIT/winter 21-22/DATA VISUALIZATION/ELA/experiments/otter-mandible-data.xlsx', 1, stringsAsFactors = TRUE)
view (otter)
otter <- na.omit(otter) # omit na
rownames(otter) <- NULL #R does not automatically re-number the rows when we drop those with NA values, we can force re-numbering
view(otter)
glimpse(otter)
pca.fit <- prcomp(otter, scale. = TRUE)



remove(x)
x= otter[,-c(1:3)]
view(x)
pca.fit <- prcomp(x, scale. = TRUE)
glimpse(x)
sapply(x, class) 
x$moment.arm.temporalis <- as.numeric(as.factor(x$moment.arm.temporalis))
x$moment.arm.masseter <- as.numeric(as.factor(x$moment.arm.masseter))
x$jaw.length <- as.numeric(as.factor(x$jaw.length))
x <- transform(x, class=as.numeric(as.character(x))) #convert all factor columns to numeric
x <- na.omit(x)
pca.summary <- summary(pca.fit)
pca.summary$rotation
biplot(pca.fit)
