#Q1)

library(MASS)
library(ggplot2)
library(tidyverse)

attach(diamonds)
view (diamonds)

diamonds_dataset <- select(diamonds, table, depth, cut, color, clarity,
                           carat, price, x, y, z)
diamonds_dataset[6:10] <- scale(diamonds_dataset[6:10])

apply(diamonds_dataset[6:10], 2, mean)

apply(diamonds_dataset[6:10], 2, sd)

set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(diamonds_dataset), replace=TRUE,
                 prob=c(0.7,0.3))
train <- diamonds_dataset[sample, ]
test <- diamonds_dataset[!sample, ] 

#Predicting cut -

model <- lda(cut~., data=train)

predicted <- predict(model, test)
names(predicted)

head(predicted$posterior)
head(predicted$x)

predicted$class<-as.ordered(predicted$class)
str(predicted$class)

mean(predicted$class==test$cut)

lda_plot <- cbind(train, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = cut))

#Predicting color -

model <- lda(color~., data=train)

predicted <- predict(model, test)
names(predicted)

head(predicted$posterior)
head(predicted$x)

predicted$class<-as.ordered(predicted$class)
str(predicted$class)

mean(predicted$class==test$color)

lda_plot <- cbind(train, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = color))

#Predicting clarity -

model <- lda(clarity~., data=train)

predicted <- predict(model, test)
names(predicted)

head(predicted$posterior)
head(predicted$x)

predicted$class<-as.ordered(predicted$class)
str(predicted$class)

mean(predicted$class==test$clarity)

lda_plot <- cbind(train, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = clarity))

#Predicting table -

model <- lda(table~., data=train)

predicted <- predict(model, test)
names(predicted)

head(predicted$class)

head(predicted$posterior)
head(predicted$x)

mean(predicted$class==test$table)

lda_plot <- cbind(train, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = table))

#Predicting depth -

model <- lda(depth~., data=train)

predicted <- predict(model, test)
names(predicted)

head(predicted$class)

head(predicted$posterior)
head(predicted$x)

mean(predicted$class==test$depth)

lda_plot <- cbind(train, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = depth))

#Q2)

library(ggplot2)
library(tidyverse)
library("ggpubr")



dataset <- diamonds

View(diamonds)

#1)

ggqqplot(dataset$carat, ylab = "Carat")

ggqqplot(dataset$price, ylab = "Price")


result1 <- cor.test(dataset$price, dataset$carat,  method="kendall")
result1

ggscatter(dataset, x = "carat", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Carat", ylab = "Price")

#2)

ggqqplot(dataset$carat, ylab = "Carat")

ggqqplot(dataset$depth, ylab = "Depth")


result1 <- cor.test(dataset$depth, dataset$carat,  method="pearson")
result1

ggscatter(dataset, x = "carat", y = "depth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Carat", ylab = "Depth")

#3)

ggqqplot(dataset$table, ylab = "Table")

ggqqplot(dataset$depth, ylab = "Depth")


result1 <- cor.test(dataset$depth, dataset$table,  method="pearson")
result1

ggscatter(dataset, x = "table", y = "depth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Table", ylab = "Depth")

#4)

ggqqplot(dataset$x, ylab = "X")

ggqqplot(dataset$y, ylab = "Y")


result1 <- cor.test(dataset$y, dataset$x,  method="pearson")
result1

ggscatter(dataset, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "X", ylab = "Y")

#5)

ggqqplot(dataset$y, ylab = "Y")

ggqqplot(dataset$z, ylab = "Z")


result1 <- cor.test(dataset$z, dataset$y,  method="pearson")
result1

ggscatter(dataset, x = "y", y = "z", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Y", ylab = "Z")


#Correlation Matrix -

#1)

dataset_num <- select(dataset, carat, depth, table, price,
                      x, y, z)

matrix_dataset <- cor(dataset_num)
head(round(matrix_dataset, 2))

library(corrplot)

corrplot(matrix_dataset, method="color")
corrplot(matrix_dataset, method="number")

#2)

corrplot(matrix_dataset, type="upper")
corrplot(matrix_dataset, type="lower")

#3)

corrplot(matrix_dataset, type="upper", order="hclust")

#4)

col<- colorRampPalette(c("black", "white", "red"))(20)
corrplot(matrix_dataset, type="upper", order="hclust", col=col)

corrplot(matrix_dataset, type="upper", order="hclust", col=c("black", "white"),
         bg="lightgreen")


#5)

library("Hmisc")
result_mat <- rcorr(as.matrix(dataset_num))
result_mat$r
result_mat$P
result_mat$P[is.na(result_mat$P)] <- 0
result_mat$P

corrplot(result_mat$r, order="hclust", 
         p.mat = result_mat$P, sig.level = 0.01)

#6)

library("PerformanceAnalytics")
perform_dataset <- dataset[, c(1,5,6,7,8,9,10)]
perform_dataset
chart.Correlation(perform_dataset, histogram=TRUE, pch=19)
