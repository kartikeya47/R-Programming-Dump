#Linear Regression
library(tidyverse)
install.packages('caret')
library(caret)
theme_set(theme_bw())




install.packages('datarium')
# Load the data
data("marketing", package = "datarium")
# Inspect the data
sample_n(marketing, 3)



# Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]



# Build the model
model <- lm(sales ~., data = train.data) #lm() is used to compute linear regression model.
# Summarize the model
summary(model)
#The summary outputs shows 6 components, including:
#Call. Shows the function call used to compute the regression model.



#Coefficients. Shows the regression beta coefficients and their statistical 
#significance. Predictor variables, that are significantly associated to 
#the outcome variable, are marked by stars.



#For a given the predictor, the t-statistic evaluates whether or not 
#there is significant association between the predictor and the outcome 
#variable, that is whether the beta coefficient of the predictor is 
#significantly different from zero.



#It can be seen that, changing in youtube and facebook advertising budget 
#are significantly associated to changes in sales while changes in newspaper 
#budget is not significantly associated with sales.



#For example, for a fixed amount of youtube and newspaper advertising 
#budget, spending an additional 1000 dollars on facebook advertising leads 
#to an increase in sales by approximately 0.1885*1000 = 189 sale units, on 
#average.



#The youtube coefficient suggests that for every 1000 dollars increase 
#in youtube advertising budget, holding all other predictors constant, we 
#can expect an increase of 0.045*1000 = 45 sales units, on average.



#We found that newspaper is not significant in the multiple regression 
#model. This means that, for a fixed amount of youtube and newspaper 
#advertising budget, changes in the newspaper advertising budget will 
#not significantly affect sales units.



#intercept indicates the location where it intersects an axis.



#As the newspaper variable is not significant, it is possible to remove 
#it from the model:
model <- lm(sales ~ youtube + facebook, data = train.data)
summary(model)
#Finally, our model equation can be written as follow: sales = 3.43+ 0.045youtube + 0.187facebook.



#goodness-of-fit
#The overall quality of the linear regression fit can be assessed using 
#the following three quantities, displayed in the model summary:
#Residual Standard Error (RSE): The RSE (or model sigma), corresponding 
#to the prediction error, represents roughly the average difference between 
#the observed outcome values and the predicted values by the model. The lower 
#the RSE the best the model fits to our data.
#R-squared (R2) and adjusted R2: The R2 measures, how well the model fits 
#the data. The higher the R2, the better the model. However, a problem with 
#the R2, is that, it will always increase when more variables are added to 
#the model, even if those variables are only weakly associated with the 
#outcome (James et al. 2014). A solution is to adjust the R2 by taking into 
#account the number of predictor variables.
#F-statistic: A large F-statistic will corresponds to a statistically 
#significant p-value (p < 0.05). In our example, the F-statistic equal 
#644 producing a p-value of 1.46e-42, which is highly significant.



#plot LR
plot(marketing$youtube,marketing$sales,
     main='Regression for youtube and sales',
     xlab='youtube',ylab='sales')
abline(lm(sales ~ youtube,data=marketing),col='red')



plot(marketing$facebook,marketing$sales,
     main='Regression for facebook and sales',
     xlab='facebook',ylab='sales')
abline(lm(sales ~ facebook,data=marketing),col='red')



plot(marketing$newspaper,marketing$sales,
     main='Regression for newspaper and sales',
     xlab='newspaper',ylab='sales')
abline(lm(sales ~ newspaper,data=marketing),col='red')



# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$sales)
# (b) R-square
R2(predictions, test.data$sales)
#From the output above, the R2 is 0.91, meaning that the observed and 
#the predicted outcome values are highly correlated, which is very good.
#The prediction error RMSE is 1.95, representing an error rate of 
#1.95/mean(test.data$sales) = 1.95/17 = 9.2%, which is good.
mean(test.data$sales)
View(marketing)
??marketing



#Logistic regression # binary
#The goal here is to model and predict if a given specimen 
#(row in dataset) is benign or malignant, based on 9 other cell features. 
install.packages("mlbench")
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
view(bc)
str(bc)



# remove id column
bc <- bc[,-1]
# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
#whenever the Class is malignant, it will be 1 else it will be 0. 
#Then, I am converting it into a factor.
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
str(bc)
table(bc$Class)



# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]



# Build Logistic Model
logitmod <- glm(Class~., family = "binomial", data=trainData)
summary(logitmod)
#glm stands for generalised linear models



pred <- predict(logitmod, newdata = testData, type = "response")
head(pred)
#if pred is greater than 0.5, it is malignant else it is benign.
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
mean(y_pred == y_act) #accuracy



##Logistic regression # multiclass
install.packages("rattle.data")
# Loading the library
library(rattle.data)
# Loading the wine data
data(wine)
# Checking the structure of wine dataset
str(wine)
with(wine, table(wine$Type, wine$Type))
# Prep Training and Test data.
library(dplyr)
# Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(wine, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- wine[-sample_id,]
# Loading the nnet package to use multinom
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(Type ~ Alcohol + Color -1, data = train)



# Checking the model
summary(multinom.fit)
# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")
table(train$precticed)
table(train$Type)



# Building classification table
ctable <- table(train$Type, train$precticed)
table(train$Type, train$precticed)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)



# Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")



# Building classification table
ctable <- table(test$Type, test$precticed)



# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
table(train$Type, train$precticed)



# Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")



# Building classification table
ctable <- table(test$Type, test$precticed)



# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
table(test$precticed)
table(test$Type)
table(test$Type, test$precticed)



#predict value using LR
youtube = 100
facebook = 100
newspaper = 10
datawine = data.frame(youtube,facebook,newspaper)
datawine
prediction <- predict(model, datawine)
prediction



#Support vector regression
#generate random data
x = 1:75
y = cumsum((rnorm(length(x))))
# plot
makePlot <-function(x,y){
  plot(x,y,col="black",pch=5,lwd=1)
  lines(x,y,lty=2, lwd=2)
  grid()}
makePlot(x,y)
title("original data")
# make data frame named `Data`
Data<-data.frame(cbind(x,y))



install.packages("e1071")
library(e1071)
# svm model 
svm_model <- svm(y ~ x , Data)
#predicted vals for all X
predictYsvm <- predict(svm_model, Data)
# viz comparison
makePlot(x,y)
title("original data  + svr")
points(Data$x, predictYsvm, col = "blue", pch=4)
points(Data$x, predictYsvm, col = "blue", type="l")
# Checking the model
summary(svm_model)



#Install Package
install.packages("hydroGOF")
#Load Library
library(hydroGOF)
RMSE(Data$y, predictYsvm)   
#comparing the result with LR
linregress_model <- lm(y ~ x, data=Data)
# make predictions for regression model for each x val
predictYlinregress <- predict(linregress_model,Data)
RMSE(Data$y, predictYlinregress)



#decision tree regression
# Install the package
install.packages("rpart")
# Load the package
library(rpart)
attach(iris)
# Create decision tree using regression
fit <- rpart(Sepal.Width ~ Sepal.Length + 
               Petal.Length + Petal.Width + Species, 
             method = "anova", data = iris)
?rpart
# Plot
plot(fit, uniform = TRUE,
     main = "Sepal Width Decision 
                 Tree using Regression")
text(fit, use.n = TRUE, cex = .7)



# Print model
print(fit)
#Deviance is a measure of goodness of fit of a generalized linear model. 
#Or rather, it's a measure of badness of fit-higher numbers indicate 
#worse fit.
#yval is the predicted response at that node.



# Create test data
df  <- data.frame (Species = 'versicolor', 
                   Sepal.Length = 5.1,
                   Petal.Length = 4.5,
                   Petal.Width = 1.4)
# Predicting sepal width
# using testing data and model
# method anova is used for regression
predict(fit, df, method = "anova")





#AIC (Akaike information criterion) is most often used for model selection. By calculating and 
#comparing the AIC scores of several possible models, you can choose 
#the one that is the best fit for the data. So if two models explain 
#the same amount of variation, the one with fewer parameters will have 
#a lower AIC score and will be the better-fit model.



library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)



my_data <- PlantGrowth
view(my_data)
# Show the levels
levels(my_data$group)
summary(my_data)
str(my_data)



# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)
#As the p-value is less than the significance level 0.05, we can conclude 
#that there are significant differences between the groups highlighted 
#with "*" in the model summary.



# 1. Homogeneity of variances
plot(res.aov, 1)
#Points 17, 15, 4 are detected as outliers, which can severely affect 
#normality and homogeneity of variance. It can be useful to remove outliers 
#to meet the test assumptions.
#The Residuals vs. Fitted plot, shows the ???tted values plotted against the 
#model residuals. If the residuals follow any particular pattern, such as 
#a diagonal line, there may be other predictors not yet in the model that 
#could improve it. The ???at lowess line looks very good as the single 
#predictor variable or regressor sufficiently explaining the dependent 
#variable.



#It's also possible to use the Levene's test to check the homogeneity of variances:
PlantGrowth %>% levene_test(weight ~ group)
#From the output above, we can see that the p-value is > 0.05, which is not 
#significant. This means that, there is not significant difference between 
#variances across groups. Therefore, we can assume the homogeneity of 
#variances in the different treatment groups.



# 2. Normality
plot(res.aov, 2)
#The normal probability plot of residuals is used to check the assumption 
#that the residuals are normally distributed. It should approximately 
#follow a straight line.
#As all the points fall approximately along this reference line, we can 
#assume normality.



# 3. Scale-Location plot
plot(res.aov, 3)
#If The lowess line that ???ts this is fairly ???at, it indicates that the spread 
#in the predictions is almost the same across the prediction line, 
#indicating the very less chances of failure of meeting the assumption



# 4. Residuals vs. Leverage plot 
plot(res.aov, 5)
#Since ctrl has one and trt1 has two outlier points stand, we can assume 
#that there are some outliers having undue influence on the ???t of the model.
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)
#Note that, in the situation where you have extreme outliers, this can be 
#due to: 1) data entry errors, measurement errors or unusual values.



#Report
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")
#A one-way ANOVA was performed to evaluate if the plant growth was 
#different for the 3 different treatment groups: ctr (n = 10), 
#trt1 (n = 10) and trt2 (n = 10).
#Plant growth decreased in trt1 group (4.66 +/- 0.79) compared to ctr 
#group (5.03 +/- 0.58). It increased in trt2 group (5.53 +/- 0.44) compared 
#to trt1 and ctr group.



#Two way annova
data("jobsatisfaction", package = "datarium")
view(jobsatisfaction)
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  get_summary_stats(score, type = "mean_sd")



bxp <- ggboxplot(
  jobsatisfaction, x = "gender", y = "score",
  color = "education_level", palette = "jco"
)
bxp



#Identify outliers in each cell design:
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  identify_outliers(score)
#There were no extreme outliers.



#Compute two-way ANOVA test
res.aov2 <- aov(score ~ gender + education_level, data = jobsatisfaction)
summary(res.aov2)
#From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:
#1. the p-value of gender is 0.234 (not significant: p>0.05).
#2. the p-value of education_level is < 2e-16 (significant), which indicates 
#that the levels of education_level are associated with significant 
#different score.



# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(score ~ gender * education_level, data = jobsatisfaction)
res.aov3 <- aov(score ~ gender + education_level + gender:education_level, data = jobsatisfaction)
summary(res.aov3)
#There was a statistically significant interaction between gender and 
#level of education for job satisfaction score, F = 7.34, p = 0.002.



# 1. Homogeneity of variances
plot(res.aov3, 1)
jobsatisfaction %>% levene_test(score ~ gender*education_level)
#The Levene's test is not significant (p > 0.05). Therefore, we can 
#assume the homogeneity of variances in the different groups.



# 2. Normality
plot(res.aov3, 2)



# 3. Scale-Location plot
plot(res.aov3, 3)



#Tukey multiple pairwise-comparisons
#As the ANOVA test is significant, we can compute Tukey HSD 
#(Tukey Honest Significant Differences, R function: TukeyHSD()) for 
#performing multiple pairwise-comparison between the means of groups.
TukeyHSD(res.aov3, which = "education_level")
#It can be seen from the output, that all pairwise comparisons are 
#significant with an adjusted p-value < 0.05.
TukeyHSD(res.aov3, which = "gender")



#Find out best fit model
res.aov1 <- aov(score ~ gender, data = jobsatisfaction)
res.aov2 <- aov(score ~ gender+education_level, data = jobsatisfaction)
install.packages('AICcmodavg')
library(AICcmodavg)
model.set <- list(res.aov1, res.aov2)
model.names <- c('res.aov1', 'res.aov2')
aictab(model.set, modnames = model.names)
#From these results, it appears that the two.way model is the best fit.
#The two-way model has the lowest AIC value



#MANOVA
#In the situation where there multiple response variables you can test 
#them simultaneously using a multivariate analysis of variance (MANOVA).
# Store the data in the variable my_data
my_data <- iris
sample_n(my_data, 10)
#We want to know if there is any significant difference, in sepal and 
#petal length, between the different species.
# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)



# Look to see which differ
summary.aov(res.man)
#From the output above, it can be seen that the two variables are highly 
#significantly different among Species.
