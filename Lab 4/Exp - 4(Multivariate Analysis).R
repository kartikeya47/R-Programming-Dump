#Multivariate statistical methods are used to analyze the joint behavior
#of more than one random variable.
#This explains that the majority of the problems in the real world are Multivariate.
#For example, we cannot predict the weather of any year based on the season.
#There are multiple factors like pollution, humidity, precipitation, etc.

#mutate(): Create or transform variables
#It is very common in data analysis to derive new variables from existing 
#variables. Here we will use the flights dataset. 
install.packages("nycflights13")
library(nycflights13)

 
library(dplyr)
?flights
data(flights)
colnames(flights)
View(flights)
flight <- flights %>% select(year, month, day, dep_delay,arr_delay, distance,
air_time )
View(flight)
#Now we will create two new variables- time_gain by subtracting arr_delay 
#from dep_delay and speed by dividing distance by air_time and multiplying with 60.
flight_mutate <- flight %>% 
  mutate(time_gain = dep_delay - arr_delay ,
         speed = distance / air_time * 60)
View(flight_mutate)

#Stacked Bar Plot
data(diamonds)
View(diamonds)

library(ggplot2)
diamonds %>%
  ggplot(aes(x = cut, fill = clarity)) + # removed y argument and value
  geom_bar() +
  ggtitle("Stacked Bar Plot") +
  xlab("CUT") +
  ylab("COUNT") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Stacked Bar Plot in same height
data_diamonds <- diamonds
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity), position = 'fill') +
 
  ggtitle("Stacked Bar Plot (same height)") +
  xlab("CUT") +
  ylab("COUNT") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Grouped Bar Plot
ggplot(data_diamonds) +
  geom_bar(aes(x = cut, fill = clarity), position = 'dodge') +
 
  ggtitle("Grouped Bar Plot") +
  xlab("CUT") +
  ylab("COUNT") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#tiles plot
ggplot(as.data.frame(table(data_diamonds$cut,
                           data_diamonds$color))) +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = Freq),
            color = "yellow") +
 
  ggtitle("Tiles Plot") +
  xlab("CUT") +
  ylab("COLOR") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#histogram
ggplot(data_diamonds) +
  geom_histogram(aes(x = price), fill = 'blue',
                 color = "lightblue", binwidth = 500)+
 
  ggtitle("Basic Histogram") +
  xlab("PRICE") +
  ylab("Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#stacked histogram
ggplot(data_diamonds) +
  geom_histogram(aes(x = price, fill = cut),
                 color = "lightblue", binwidth = 500)+
 
  ggtitle("Stacked Histogram") +
  xlab("PRICE") +
  ylab("Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#violin plot
ggplot(data_diamonds) +
  geom_violin(aes(x = cut , y = carat, fill = cut)) +
 
  ggtitle("Violin Plot") +
  xlab("CUT") +
  ylab("CARAT") +
  theme_bw() +
 
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Ridge Plot
install.packages("ggridges")
library(ggridges)
ggplot(data_diamonds) +
  geom_density_ridges(aes(x = carat , y = cut,
                          fill = clarity), alpha = 0.7) +
 
  ggtitle("Ridge Plot") +
  xlab("carat") +
  ylab("CUT") +
  theme_bw() +
 
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#working with colors
attach(mtcars)
sp <- p + geom_point(aes(color = gear))
sp
#change color manually
sp + scale_color_manual(values=c("red", "green", "blue"))
sp + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#Using RColorBrewer palettes
install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()
#There are 3 types of palettes : sequential, diverging, and qualitative.
#Sequential palettes are suited to ordered data that progress from low to 
#high (gradient). The palettes names are : Blues, BuGn, BuPu, GnBu, Greens, 
#Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu 
#YlOrBr, YlOrRd.
#Diverging palettes put equal emphasis on mid-range critical values and 
#extremes at both ends of the data range. The diverging palettes are : 
#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
#Qualitative palettes are best suited to representing nominal or categorical 
#data. They not imply magnitude differences between groups. The palettes names 
#are : Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

 

# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'RdBu')
sp + scale_color_brewer(palette="RdBu")

 

#Use gray colors
sp + scale_color_grey() + theme_classic()

 

#Gradient colors for scatter plots
sp2<-ggplot(mtcars, aes(x=wt, y=mpg, color=qsec)) + geom_point()
sp2
sp2+scale_color_gradient(low="blue", high="red") # Sequential color scheme
# Diverging color scheme
mid<-mean(mtcars$qsec)
sp2+scale_color_gradient2(midpoint=mid, low="blue", mid="white",
                          high="red", space ="Lab" )
?scale_color_gradient2
# Gradient between n colors
sp2+scale_color_gradientn(colours = rainbow(5))
# Use rainbow colors
barplot(1:5, col=rainbow(5))
# Use heat.colors
barplot(1:5, col=heat.colors(5))
# Use terrain.colors
barplot(1:5, col=terrain.colors(5))
# Use topo.colors
barplot(1:5, col=topo.colors(5))
# Use cm.colors
barplot(1:5, col=cm.colors(5))

