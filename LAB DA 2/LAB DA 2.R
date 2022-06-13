#Question 1

install.packages("nycflights13")
library(nycflights13)

data(flights)
View(flights)

colnames(flights)

flights_1 <- select(flights, carrier, origin, air_time,
                    dep_time, sched_dep_time, year, distance)

flights_1 = na.omit(flights_1)

flights_1

ggplot(flights_1, aes(fill=origin, x=carrier)) +
  geom_bar() +
  ggtitle("Q1) (a) Bar Plot") +
  xlab("Carrier") +
  ylab("Count")


ggplot(flights_1) +
  geom_point(aes(x = sched_dep_time, y = dep_time, color=factor(origin)))+
  ggtitle("Q1) (b) Scatter Plot") +
  xlab("Scheduled Departure Time") +
  ylab("Departure Time")

ggplot(flights_1, aes(x = distance, y = air_time)) +
  geom_line(color="#69b3a2", size=2, alpha=0.9)+
  ggtitle("Q1) (c) Line Plot") +
  xlab("Distance Travelled") +
  ylab("Air Time")

ggplot(flights_1) +
  geom_histogram(aes(x = air_time, fill = carrier), binwidth = 100) +
  ggtitle("Q1) (d) Histogram") +
  xlab("Air Time") +
  ylab("Frequency")

ggplot(flights_1) +
  geom_density_ridges(aes(x = air_time , y = carrier,
                          fill = origin), alpha = 0.7) +
  ggtitle("Q1) (e) Ridge Plot") +
  xlab("Air Time") +
  ylab("Carrier")

ggplot(flights_1) +
  geom_violin(aes(x = carrier , y = air_time, fill = origin)) +
  
  ggtitle("Q1) (f) Violin Plot") +
  xlab("Carrier") +
  ylab("Air Time")

#Question 2

data(flights)

flights_2 <- na.omit(flights)

flights_2 <- select(flights_2, flight, day, dep_delay,
                    arr_delay, air_time, distance, hour, minute)

pca_res <- prcomp(flights_2, scale = TRUE)
pca_res
pca_res$rotation

pca_res$rotation <- -1*pca_res$rotation

pca_res$rotation

pca_res$x

pca_res$x <- -1*pca_res$x

head(pca_res$x)

biplot(pca_res, scale = 0)

total_var = pca_res$sdev^2 / sum(pca_res$sdev^2)

qplot(c(1:8), total_var) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

biplot(pca_res,choices=c(1,2))
