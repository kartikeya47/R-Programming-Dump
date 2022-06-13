#Q1

df_traffic = read.csv('D:/website-traffic.csv')
View(df_traffic)

df_temp = read.csv('D:/room-temperature.csv')
View(df_temp)

#Q2

library(dplyr)

df1 = filter(df_traffic, DayOfWeek == 'Monday')
m1 = mean(df1$Visits)
df2 = filter(df_traffic, DayOfWeek == 'Tuesday')
m2 = mean(df2$Visits)
df3 = filter(df_traffic, DayOfWeek == 'Wednesday')
m3 = mean(df1$Visits)
df4 = filter(df_traffic, DayOfWeek == 'Thursday')
m4 = mean(df4$Visits)
df5 = filter(df_traffic, DayOfWeek == 'Friday')
m5 = mean(df5$Visits)
df6 = filter(df_traffic, DayOfWeek == 'Saturday')
m6 = mean(df6$Visits)
df7= filter(df_traffic, DayOfWeek == 'Sunday')
m7 = mean(df7$Visits)

day_vect <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
              'Saturday', 'Sunday')
visits_vec <- c(m1, m2, m3, m4, m5, m6, m7)

barplot(visits_vec, names.arg = day_vect, main = 'Question 2',xlab = 'Days',
        ylab = 'Average of Visits', col= 'red',horiz = FALSE)

#Q3

library(ggplot2)

modified_monthday <- array(df_traffic$MonthDay)

modified_monthday <- gsub('[0-9]*','',modified_monthday)

modified_monthday

df_dummy = df_traffic

df_dummy$Month <- modified_monthday

ggplot(df_dummy,aes(DayOfWeek, Visits, color=factor(Month),
shape=factor(Month))) + scale_shape_manual(values=1:nlevels(factor(df_dummy$Month)))+ 
labs(title = "Question 3", x="Day of Week", y="Visits") + geom_point()

#Q4


boxplot(df_temp[-1], col = 'blue', main = "Question 4", xlab = "House Parts",
ylab = "Temperatures")

#Q5
library(tidyr)
library(ggplot2)

modified_data <- gather(df_temp, "PartsOfTheRoom", "Temparatures", 2,3,4,5)

ggplot(modified_data) + geom_density(aes(x = Temparatures,
fill = PartsOfTheRoom), alpha = 0.6) + facet_wrap(~PartsOfTheRoom)+
labs(title = "Question 5")
