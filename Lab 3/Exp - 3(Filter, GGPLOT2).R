#stripchart
View(mtcars)
disp = mtcars
stripchart(disp)
stripchart(disp,method="stack")
stripchart(disp,method="jitter")
stripchart(disp,vertical=TRUE)
stripchart(disp,vertical=TRUE,method="jitter")
stripchart(disp,method="stack",
           main='stripchart',
           xlab='disp')



#quantile-quantile plot: to check that the data is normally distributed or not
qqnorm(mtcars$mpg)

qqnorm(mpg,
       main="Normal Q-Q Plot of mpg",
       ylab="Sample Quantiles mpg")

#DATA TRANSFORMATIONS (arrange(), select(), filter(), gather(), spread(), group_by() & summarize(), mutate())
library(tidyverse)
data(mpg)
View(mpg)
#Arrange()
data <- drop_na(mpg)
View(data)
mpg_arrange1 <- mpg %>% arrange(displ) #order the observations as per the displ column
View(mpg_arrange1)

count(data, cyl)


mpg_arrange2 <- mpg %>% arrange(displ, cty, hwy)
View(mpg_arrange2) #We can see the second(cty) and the third(hwy) columns are breaking the ties in the values of the first(displ) and second(cty) columns respectively.



mpg_arrange3 <- mpg %>% arrange(desc(displ)) #To order the observations in descending order, the desc() function will be used
View(mpg_arrange3)
#if there were any missing values then these would always be sorted at the end



#Select()
#select three columns - displ, cty & hwy to a new data frame
mpg_select1 <- mpg %>% select(displ, cty, hwy)
View(mpg_select1)



#boxplot
#The thick center line on each boxplot is the median (50th percentile) 
#of that variable. The top and bottom edges of the box are the 75th 
#and 25th percentile, respectively. If the data are from a symmetric 
#distribution, such as the  or normal distribution, then the median 
#should be approximately centered with respect to those 2 percentiles. 
#The fact that is if the median is closer to upper edge, indicates that 
#the data are skewed to the right (mean>median) and if median closer to the 
#lower edge indicates that the data are skewed to the left (mean<median)
boxplot(mpg_select1)




#select all columns from displ to hwy
mpg_select2 <- mpg %>% select(displ : hwy)
View(mpg_select2)



#another way to select
# All Rows and All Columns
mpg[,]



# First row and all columns
mpg[1,]



# First two rows and all columns
mpg[1:2,]



# First and third row and all columns
mpg[ c(1,3), ]



# First Row and 2nd and third column
mpg[1, 2:3]



# First, Second Row and Second and Third COlumn
mpg[1:2, 2:3]



# Just First Column with All rows
mpg[, 1]



# First and Third Column with All rows
mpg[,c(1,3)]

#filter()
?diamonds
data(diamonds)
View(diamonds)
#We can check summary of the columns values
summary(diamonds$carat)
#filter only the Ideal cut diamonds
diamonds_filter1 <- diamonds %>% filter(cut == 'Ideal')
View(diamonds_filter1)
#filter Ideal & Premium cut diamonds
diamonds_filter2 <- diamonds %>% filter(cut == c('Ideal', 'Premium'))
View(diamonds_filter2)
#filter diamonds which price is greater than 2500
diamonds_filter3 <- diamonds %>% filter(price > 2500)
View(diamonds_filter3)
# filter in three columns : Ideal cut 0.54 carat diamonds with price 1266
diamonds_filter4 <- diamonds %>%
  filter(cut == 'Ideal', carat == 0.54, price == 1266)
View(diamonds_filter4)
#more complex filtering criteria - Ideal cut, between 0.4 and(&) 1.0 carat
#with price less than 580 or(|) greater than 10000.
diamonds_filter5 <- diamonds %>%
  filter(cut == 'Ideal' ,
         carat >= 0.4 & carat <= 1.0 ,
         price < 580 | price > 10000)
View(diamonds_filter5)
#filter with not(!) operator: !(x & y) is the same as !x | !y, and !(x | y) is the same as !x & !y.
diamonds_filter6 <- diamonds %>%
  filter(cut != 'Ideal',
         !(carat >= 0.4 & carat <= 1.0) ,
         !(price < 580 | price > 10000))
View(diamonds_filter6)

#gather()
#Sometimes we can have a dataset where the observations are found in
#column names that need to be gathered under a variable with a new column name.
#create the dataset
Country_Name <- c('Qatar', 'United States', 'Germany', 'Canada', 'United Kingdom')
Y2009 <- c(59094, 47099, 41732, 40773, 38454)
Y2010 <- c(67403, 48466, 41785, 47450, 39080)
Y2011 <- c(82409, 49883, 46810, 52101, 41652)
gdp <- data.frame(Country_Name, Y2009, Y2010, Y2011)
view(gdp)
#Y2009, Y2010 & Y2011 these column names should be under a single variable or column name- 'Year'.
gdp_gather <- gdp %>% gather("Year", "GDP" , 2:4)
View(gdp_gather)
#To make this dataset ready for numerical analysis we need to remove the
#character "Y"(or any character) from the Year values and convert it from
#character to integer.
#gsub(pattern, replacement, x)
gdp_gather$Year <- gsub("[a-zA-Z]", "", gdp_gather$Year)
gdp_gather$Year <- as.integer(gdp_gather$Year)
View(gdp_gather)
glimpse(gdp_gather)

attach(mtcars)
mtcars_data <- mtcars
view(mtcars_data)
mtcars_data$cyl <- gsub("[[:digit:]]+", "", mtcars_data$cyl)

#spread()
#Sometimes we can see variables are distributed in observations
#in a dataset. In this case we need to spread it to column names.
#Lets build a dataset.
Student <- c('Jack', 'Jack', 'Jack', 'Jack', 'Jack', 'Jack',
             'Rose', 'Rose', 'Rose', 'Rose', 'Rose', 'Rose')
Subject <- c('English', 'Biology', 'Chemistry', 'Maths', 'Physics',
             'Social Science', 'English', 'Biology', 'Chemistry',
             'Maths', 'Physics', 'Social Science')
Marks <- c(80, 70, 87, 75, 90, 82, 65, 70, 88, 92, 79, 93)
reportCard <- data.frame(Student, Subject, Marks)
View(reportCard)
#In this report card dataset, if we consider the Subject names as variables
#then it need to spread out to the column names.
reportCard_spread1 <- reportCard %>% spread(Subject, Marks)
View(reportCard_spread1)
#If we consider Student names as variables
reportCard_spread2 <- reportCard %>% spread(Student, Marks)
View(reportCard_spread2)

#group_by() & summarize()
?msleep
data(msleep)
colnames(msleep)
msleep <- msleep %>% select(name, order, sleep_total, bodywt)
View(msleep)
#We will summarize sleep_total to its average values- sleep_avg
#and bodywt to its maximum values- bodywt_max. This summarization
#will be grouped as per order and the number of each order observations
#will be under count column. All these will be done by group_by() and
#summarize() with mathematical functions- n(), mean() and max().
msleep_groupby_summarize <- msleep %>% group_by(order) %>%
  summarise(
    count = n(),
    sleep_avg = mean(sleep_total),
    bodywt_max = max(bodywt)
  )
View(msleep_groupby_summarize)

#Basic line plot
view(mtcars)
ggplot(mtcars %>% filter(am == "Automatic")) +
  geom_line(aes(x = mpg, y = disp), 
            lwd = 1.25, color = 'darkgreen') +
  
  ggtitle("Basic Line Plot", subtitle = "name - mtcars") +
  xlab("mpg") +
  ylab("disp") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))



#Line Plot with feature differentiation by color & line type
ggplot(mtcars) +
  geom_line(aes(x = mpg, y = disp, 
                color = am, lty = am), lwd = 1.25) +
  
  ggtitle("Line Plot with feature differentiation") +
  xlab("mpg") +
  ylab("disp") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Basic Bar Plot
view(diamonds)
diamonds %>% 
  ggplot(aes(x = cut, y = mean(price), fill = cut)) +
  geom_bar(stat = "identity") +
  
  ggtitle("Basic Bar Plot") +
  xlab("CUT") +
  ylab("price") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))



# graphing the frequency/count of the cut categories
diamonds %>% 
  ggplot(aes(x = cut, fill = cut)) + # removed y argument and value
  geom_bar()
#is same as
diamonds %>% 
  ggplot(aes(x = cut, fill = cut)) + # removed y argument and value
  geom_bar(stat = "count")
#NOTE: If it is stat = "identity", we are asking R to use the 
#y-value we provide for the dependent variable. If we specify 
#stat = "count" or leave geom_bar() blank, R will count the number 
#of observations based on the x-variable groupings.

# this calculates the total cost of all diamonds within each clarity category
diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_bar(stat = "identity")

#density plot
#A density plot is a representation of the distribution of a 
#numeric variable. It uses a kernel density estimate to show the 
#probability density function of the variable (see more). It is a 
#smoothed version of the histogram and is used in the same concept.
ggplot(data_diamonds) +
  geom_density(aes(x = price, 
                   fill = cut), alpha = 0.7)+
  
  ggtitle("Density Plot") +
  xlab("price") +
  ylab("Density") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

mtcars
View(mtcars)
dataneeded<-mtcars[c(3,10)]
View(dataneeded)
ggplot(dataneeded) +
  geom_density(aes(x = disp,
                   fill = gear), alpha = 0.4) +
  facet_wrap(~gear)

data_diamonds <- diamonds

#read csv file
web = read.csv('F:/VIT/winter 21-22/DATA VISUALIZATION/ELA/experiments/website-traffic.csv')