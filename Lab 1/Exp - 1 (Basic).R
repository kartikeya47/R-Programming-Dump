# The first lines of code in an R script are dedicated to loading the libraries we will use.



# install "dslabs" package from tools->install packages.



#We can then load the package into our R sessions using the library function:
library(dslabs)



# Once you install a package, it remains installed and only needs 
# to be loaded with library. The package remains loaded until we quit 
# the R session. If you try to load a package and get an error, it 
# probably means you need to install it first.



# Install using command
install.packages("tidyverse")




# You can see all the packages you have installed using the following function:
installed.packages()



#Define variables: We use <- to assign values to the variables.
a <- 1
b <- 1
c <- -1



# to see the value of a
a
print(a)



# You can see all the variables saved in your workspace by typing:
ls()



(-b + sqrt(b^2 - 4*a*c) ) / ( 2*a )



#to see help
?log
args(log)



log(8, base = 2)



?"+"



# You can see all the available datasets by typing:
data()



#These datasets are objects that can be used by simply typing the name. For example, if you type:
co2



# Values remain in the workspace until you end your session or erase them with the function rm.



# to see datatype
class(a)



## A large proportion of data analysis challenges start with data stored in a data frame.
library(dslabs)
data(murders)
class(murders)

#The function str is useful for finding out more about the structure of an object:
str(murders)



# We can show the first six lines using the function head:
head(murders)



#we use the accessor operator $ in the following way:
murders$population
murders$p
population



# We can quickly access the variable names using:
names(murders)



pop <- murders$population
length(pop)
length(murders$abb)
class(pop)
class(murders$state)
class(murders$region)

z <- 3 == 2
z
class(z)



# Factors are useful for storing categorical data.
class(murders$region)
levels(murders$region)
#In the background, R stores these levels as integers and keeps a 
#map to keep track of the labels. This is more memory efficient 
#than storing all the characters.



#plot data
summary(murders)
plot(murders$region) #R chooses the best plot for the particular type of data with plot() function
# points and lines 
plot(murders$population, type= "p") #only the points
plot(murders$population, type= "l") #only the lines
plot(murders$population, type= "b") #both points and lines
plot(murders$population)
plot(murders$region, type= "p") #for the particular data point representation is non - suitable
# high density vertical lines.
plot(murders$population, type= "h")
plot(murders$region, murders$population)
#labels and title
plot(murders$region, xlab = 'names', ylab = 'No of Instances', main = 'region plot', col = 'green')
# Horizontal bar plot
barplot(murders$population, main = 'population plot',xlab = 'values', ylab = 'No of Instances', col= 'green',horiz = TRUE)
# Vertical bar plot
barplot(murders$population, main = 'population plot',xlab = 'No of Instances', ylab = 'values', col= 'green',horiz = FALSE)
#histogram: A histogram represents the frequencies of values 
#of a variable bucketed into ranges
hist(murders$population)
hist(murders$population, main = 'population',xlab = 'values', col='red')
#box plot
boxplot(murders$population)



#grid of charts
#the margin of the grid(mar), no of rows and columns(mfrow), 
#whether a border is to be included(bty) and position of the 
#labels(las: 1 for horizontal, las: 0 for vertical)
par(mfrow=c(3,3), mar=c(2,5,2,1), las=0, bty="n") #3*3 = 9 cells = 9 plots
plot(murders$region)
plot(murders$population, type= "p")
plot(murders$population, type= "l")
plot(murders$population, type= "b")
plot(murders$region, murders$population)
hist(murders$population, main = 'population',xlab = 'values', col='red')
barplot(murders$population, main = 'population plot',xlab = 'values', ylab = 'No of Instances', col= 'green',horiz = TRUE)
plot(murders$population, type= "h")
boxplot(murders$region)



#What happens when we use plot command with the entire 
#dataset without selecting any particular columns?
#We get a matrix of scatterplots which is a correlation 
#matrix of all the columns. 
plot(murders)



# Suppose we want the levels of the region by the total number 
#of murders rather than alphabetical order. If there are values 
#associated with each level, we can use the reorder and specify 
#a data summary to determine the order.
region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)



#Data frames are a special case of lists. Lists are useful because you 
#can store any combination of different types.
record <- list(name = "John Doe",
               student_id = 1234,
               grades = c(95, 82, 91, 97, 93),
               final_grade = "A")
record
class(record)



# As with data frames, you can extract the components of a list with the accessor $.
record$student_id
record[["student_id"]] #we can also use [[]]

# You might also encounter lists without variable names.
record2 <- list("John Doe", 1234)
record2
#If a list does not have names, you cannot extract the elements 
#with $, but you can still use the brackets method and instead of 
#providing the variable name, you provide the list index
record2[[1]]