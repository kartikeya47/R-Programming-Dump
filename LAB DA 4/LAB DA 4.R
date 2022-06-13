#Q1)

library(tidyverse)
library(caret)

dataset <- read.csv("D:/Winter Semester 2021 - 2022/Lecture Material/Data Visualization/Lab/LAB DA 4/harvest.csv")

view(dataset)

sample_n(dataset, 10)

set.seed(123)
dependent_var <- createDataPartition(dataset$yield, p = 0.8, list = FALSE)


train_data  <- dataset[dependent_var, ]
test_data <- dataset[-dependent_var, ]

model <- lm(yield ~., data = train_data)
summary(model)

plot(dataset$density, dataset$yield,
     main='Regression for Density and Yield',
     xlab='Density',ylab='Yield')
abline(lm(yield ~ density, data = dataset), col='red')

plot(dataset$block, dataset$yield,
     main='Regression for Block and Yield',
     xlab='Block',ylab='Yield')
abline(lm(yield ~ block, data = dataset), col='red')

plot(dataset$fertilizer, dataset$yield,
     main='Regression for Fertilizer and Yield',
     xlab='Fertilizer',ylab='Yield')
abline(lm(yield ~ fertilizer, data = dataset), col='red')

density <- 2
block <- 1
fertilizer <- 3
yield <- 175.5
dummy_data <- data.frame(density, block, fertilizer, yield)

predds <- predict(model, test_data)

RMSE(predds, test_data$yield)
R2(predds, test_data$yield)

pred <- predict(model, dummy_data)
pred


#Q2)

library(dplyr)
with(dataset, table(dataset$density, dataset$density))
training_dataset <- sample_frac(dataset, 0.7)
sample_id <- as.numeric(rownames(training_dataset)) 
test_dataset <- dataset[-sample_id,]

require(nnet)

logistic.fit <- multinom(density ~ block + fertilizer -1, data = training_dataset)

summary(logistic.fit)

training_dataset$precticed <- predict(logistic.fit, newdata = training_dataset, "class")
table(training_dataset$precticed)
table(training_dataset$density)


ctable <- table(training_dataset$density, training_dataset$precticed)
table(training_dataset$density, training_dataset$precticed)

round((sum(diag(ctable))/sum(ctable))*100,2)


test_dataset$precticed <- predict(logistic.fit, newdata = test_dataset, "class")


ctable <- table(test_dataset$density, test_dataset$precticed)


round((sum(diag(ctable))/sum(ctable))*100,2)
table(training_dataset$density, training_dataset$precticed)

density <- 2
block <- 1
fertilizer <- 3
yield <- 175.5
dummy_data <- data.frame(density, block, fertilizer, yield)

predds <- predict(logistic.fit, newdata = dummy_data, "class")
predds


#Q3)

library(e1071)

svm_mod <- svm(yield ~ density , dataset)

density <- 2
block <- 1
fertilizer <- 3
yield <- 175.5
dummy_data <- data.frame(density, yield)

predicted <- predict(svm_mod, dataset)

plot(dataset$density, dataset$yield)
title("Orignal Data  + SVR")
points(dataset$density, predicted, col = "blue", pch = 4)
points(dataset$density, predicted, col = "blue", type = "l")

summary(svm_mod)

predds <- predict(svm_mod, dummy_data)
predds


#Q4)

library(rpart)

fit_data <- rpart(density ~ block + 
               fertilizer, method = "anova", data = dataset)

plot(fit_data, uniform = TRUE,
     main = "Yield Decision Tree using Regression")
text(fit_data, use.n = TRUE, cex = .7)


print(fit_data)

density <- 2
block <- 1
fertilizer <- 3
yield <- 175.5
dummy_data <- data.frame(density, block, fertilizer, yield)


predict(fit_data, dummy_data, method = "anova")


#Q5)

library(AICcmodavg)

model_1 <- lm(yield ~., data = train_data)
model_2 <- multinom(density ~ block + fertilizer -1, data = training_dataset)
model_3 <- svm(yield ~ density , dataset)

model_list <- list()

model_list[[1]] <- model_3
model_list[[2]] <- model_3
model_list[[3]] <- model_3

model_names <- c("Linear Regression", "Logistic Regression", "Decision Tree")

aictab(cand.set = model_list, modnames = model_names)
