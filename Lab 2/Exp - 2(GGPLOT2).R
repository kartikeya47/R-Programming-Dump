#Matrices are another type of object that are common in R. 
#Matrices are similar to data frames in that they are two-dimensional: 
#they have rows and columns.
mat <- matrix(1:12, 4, 3)
mat
mat[2, 3]
mat[2, ]
mat[, 3]
mat[, 2:3]
mat[1:2, 2:3]



#We can convert matrices into data frames using the function
as.data.frame(mat)
# in matrices data have to be all the same type. For this reason data 
#frames are much more useful for storing data, since we can have 
#characters, factors, and numbers in them.



#We can create vectors using the function c, which stands for concatenate
codes <- c(380, 124, 818)
codes
#install.packages("tidyverse")
library(tidyverse)
view(codes)
country <- c("italy", "canada", "egypt")
view(country)
country1 <- c('italy', 'canada', 'egypt')
country1
misc <- c('italy', 'canada', 'egypt', 500)
view(misc)



#Sometimes it is useful to name the entries of a vector. For example, 
#when defining a vector of country codes, we can use the names to 
#connect the two:
codes <- c(italy = 380, canada = 124, egypt = 818)
codes
class(codes)
names(codes)



#Another useful function for creating vectors generates sequences:
seq(1, 10)
seq(1, 10, 2)
1:10
class(1:10)
class(seq(1, 10, 0.5))



#We use square brackets to access specific elements of a vector.
codes[2]
codes[c(1,3)]
codes[1:2]
codes[c("egypt","italy")]



#coercion is an attempt by R to be flexible with data types
x <- c(1, "canada", 3)
x
class(x)



x <- 1:5
y <- as.character(x)
y
as.numeric(y)



#When a function tries to coerce one type to another and encounters 
#an impossible case, it usually gives us a warning and turns the entry 
#into a special value called an NA for "not available."
x <- c("1", "b", "3")
class(x)
as.numeric(x)
h <- c("1", "3")
class(h)
as.numeric(h)



#Say we want to rank the states from least to most gun murders. 
#The function sort sorts a vector in increasing order. We can therefore 
#see the largest number of gun murders by typing:
library(dslabs)
data(murders)
sort(murders$total)



#use airquality
data(airquality)
view(airquality)
summary(airquality)
par(mfrow=c(1,1))
plot(airquality)

# Multiple box plots -

#If the median line is close to the upper part of the box, then mean > median (right-skewed)
#If the median line is close to the lower part of the box, then mean < median (left-skewed data)
#If the median line is exactly at the middle, then there is a normal/symmetric distribution

boxplot(airquality[,0:3], main='Multiple Box plots')



# Installing & Loading the package 'lattice'
install.packages("lattice")
library(lattice)  
#Loading the dataset
#The attach function attaches the database to the R 
#search path so the objects in the database can be accessed by 
#simply giving their names.
attach(mtcars)
View(mtcars)
head(mtcars)
gear
#since we have attached the dataset mtcars, we do not need to 
#specify mtcars$gear or mtcars$cyl
gear_factor<-factor(gear,levels=c(3,4,5),
                    labels=c("3gears","4gears","5gears")) 
cyl_factor <-factor(cyl,levels=c(4,6,8),
                    labels=c("4cyl","6cyl","8cyl"))

cyl
densityplot(cyl, main="Density Plot", xlab="Miles per Gallon")
splom(mtcars[c(1,3,4,5,6)], main="MTCARS DATA")

attach(mtcars)

mtcars$gear <- factor(mtcars$gear, levels = c(3,4,5), labels = c("3gears", "4gears", "5gears"))
mtcars$am <- factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))
mtcars$cyl <- factor(mtcars$cyl, levels = c(4,6,8), labels = c("4cyl", "6cyl", "8cyl"))

p <- ggplot(data = mtcars, aes(x = wt, mpg))
#Plot has been created, now layers could be added
#Installing & Loading the package 'ggplot2'
install.packages("ggplot2") 
library(ggplot2)
#Loading the dataset
attach(mtcars)



# create factors with value labels 
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),  
                      labels=c("3gears", "4gears", "5gears"))  
mtcars$am <- factor(mtcars$am,levels=c(0,1),  
                    labels=c("Automatic","Manual"))  
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),  
                     labels=c("4cyl","6cyl","8cyl"))



#scatterplots: geom_point() is used to create scatterplots.
# aes map variables in the data to visual properpeties of ggplot 
#geoms (points, bars, box plot, etc). These visual caracteristics are 
#known as aesthetics (or aes) and include: color and fill. 
#points shape.

p <- ggplot(data = mtcars, aes(x = wt, mpg))
p + geom_point()
p + geom_point(aes(color = qsec))
p + geom_point(aes(alpha = qsec))
p + geom_point(aes(size = qsec))
p + geom_point(aes(color = disp))
p + geom_point(aes(color = factor(disp)))
p + geom_point(aes(shape = factor(disp)))
p + geom_point(aes(shape = factor(disp), color = factor(disp)))
p + geom_point(aes(shape = factor(qsec), color = factor(disp)))
p + geom_point(aes(shape = factor(carb), color = factor(disp)))
p + geom_point(aes(shape = factor(carb), color = factor(disp)), size =2.25)
p + geom_point(aes(shape = factor(carb), color = factor(disp)), size =2.25) + theme_bw()
p + geom_point(aes(shape = factor(carb), color = factor(disp)), size =2.25) + theme_bw() + theme(axis.title = element_text(size=rel(1.5)))

