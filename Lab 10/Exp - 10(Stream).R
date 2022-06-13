library(tidyverse)
library(stream)
set.seed(1000)
#DSD_Gaussians generates randomly placed static clusters with random multivariate Gaussian distributions. Allows generating and marking outliers for outlier
#detectors.
stream <- DSD_Gaussians(k = 3, d = 2)
plot(stream)
?DSD_Gaussians
#After loading stream, we create a simulated data stream with
#data points drawn from three random Gaussians in 2D space. Note that we set the random
#number generator seed every time when we create simulated data sets to get reproducible
#results.



dstream <- DSC_DStream(gridsize = .1, Cm = 1.2)
?DSC_DStream
update(dstream, stream, n = 500)
dstream
plot(dstream)
#we create an instance of the density-based data stream clustering algorithm D-Stream
#which uses grid cells as micro-clusters. We specify the grid cell size (gridsize) as .1 and
#require that the density of a grid cell (Cm) needs to be at least 1.2 times the average cell
#density to become a micro-cluster. Then we update the model with the next 500 data points
#from the stream.



km <- DSC_Kmeans(k = 3)
recluster(km, dstream)
plot(km, stream, type = "both")
#Finally, we perform reclustering using k-means with three clusters and plot the resulting micro
#and macro clusters.
#Data stream clustering result of D-Stream on a simple simulated data set with
#three random Gaussians. Micro-clusters are shown as circles and macro-clusters are shown
#as crosses (size represents weight).



# the stream framework consists of two main components:
#1. Data stream data (DSD) simulates or connects to a data stream.
#2. Data stream task (DST) performs a data stream mining task. In the example above,
#we performed twice a data stream clustering (DSC) task.



#Simulated streams with static structure.
#DSD_BarsAndGaussians generates two uniformly filled rectangular and two Gaussians clusters with different density.
stream2 <- DSD_BarsAndGaussians(angle=45, noise=0.1)
?DSD_BarsAndGaussians
plot(stream2)



#DSD_mlbenchData provides streaming access to machine learning benchmark data
#sets found in the mlbench package
?DSD_mlbenchData
stream3 <- DSD_mlbenchData("Shuttle")
stream3
plot(stream3, n=100)



#DSD_mlbenchGenerator interfaces the generators for artificial data sets defined in
#the mlbench package.
?DSD_mlbenchGenerator
stream4 <- DSD_mlbenchGenerator(method="cassini")
plot(stream4, n=500)
stream4
library("mlbench")
set.seed(1234)
Cassini <- mlbench.cassini(1000)
view(Cassini)



#DSD_Target generates a ball in circle data set.
?DSD_Target
# create data stream with three clusters in 2D
stream5 <- DSD_Target()
# plotting the data
plot(stream5)



#DSD_UniformNoise generates uniform noise in a d-dimensional (hyper) cube.
?DSD_UniformNoise
# create data stream with three clusters in 2D
stream6 <- DSD_UniformNoise(d=2)
plot(stream6, n=100)
# specify a different range for each dimension 
stream7 <- DSD_UniformNoise(d=3, range=rbind(c(0,1), c(0,10), c(0,5)))
plot(stream7, n=100)




#Creating a stream
set.seed(1000)
stream8 <- DSD_Gaussians(k = 3, d = 3, noise = .05, p = c(.5, .3, .1))
stream8
#New data points are requested from the stream using get_points(). When a new data point
#is requested from this generator, a cluster is chosen randomly (using the probability weights
#in p)
p <- get_points(stream8, n = 5)
p



#Many generators also return the ground truth (class or cluster label) if they are called with class
#= TRUE.
p <- get_points(stream8, n = 100, class = TRUE)
head(p, n = 10)
#The data was created by a generator with 5% noise. Noise points do not belong to
#any cluster and thus have a class label of NA.



#Next, we plot 500 points from the data stream to get an idea about its structure
plot(stream8, n = 500)
#Noise points are plotted as gray dots. 



#The data can also be projected on its first two principal
#components using method="pc".
plot(stream8, n = 500, method = "pc")





#Stream also supports data streams which contain concept drift. Several examples of such
#data stream generators are collected in DSD_Benchmark. We create an instance of the first
#benchmark generator which creates two clusters moving in two-dimensional space. One moves
#from top left to bottom right and the other one moves from bottom left to top right. Both
#clusters overlap when they meet exactly in the center of the data space.
set.seed(1000)
stream <- DSD_Benchmark(1)
stream



#An animation of the data can be generated using
#animate_data(). We use reset_stream() to start the animation at the beginning of the
#stream.
library('animation')
reset_stream(stream)
animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
?animate_data



#can be replayed using ani.replay().
animation::ani.options(interval = .1)
ani.replay()



#Animations can also be saved as an animation embedded in a HTML document or an animated
#image in the Graphics Interchange Format (GIF) which can easily be used in presentations.
saveHTML(ani.replay())
saveGIF(ani.replay())




#Outlier generating data streams
#We can use DSD_Gaussians to generate and mark outliers as well. We define a data stream
#consisting of 10000 data points.
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, outliers = 4, outlier_options = list(outlier_horizon = 10000), separation = 0.3, space_limit = c(0,1))



#Next, we plot 10000 points from the data stream.
#We can obtain data points from the stream, asking for the outlier marks
reset_stream(stream)
p <- get_points(stream, n = 10000, outlier = TRUE)
head(p)



#Outlier marks can be retrieved from the outlier attribute
out_marks <- attr(p, "outlier")
sum(out_marks)



#We can see that four data points were marked as outliers. These outliers were generated at
#positions
which(out_marks)
#Such data stream generators can be used in outlier detector
#assessment, since they generate the ground truth for the true positive outliers.



#Advanced statistical data streams
#Maximal variance and space limitations
set.seed(1000)
stream1 <- DSD_Gaussians(k = 3, d = 2, variance_limit = 0.2, space_limit = c(0, 5))
plot(stream1)



set.seed(1000)
stream2 <- DSD_Gaussians(k = 3, d = 2, variance_limit = 2, space_limit = c(0, 5))
plot(stream2)
#we can experience overlapping of clusters due to high maximal variance limit.



#Keeping clusters sufficiently separated
#To keep cluster from overlapping we can use two separation distance measures: Euclidean and
#Mahalanobis (the statistical distance). While Euclidean distance can be used to some extent,
#it might not keep clusters cleanly separated at all times, since cluster size highly depend on
#the related covariance matrix. This is the reason why we want do use statistical distance
#(Mahalanobis) to control cluster separation.
set.seed(1000)
stream1 <- DSD_Gaussians(k = 5, d = 2, variance_limit = 0.2,
                         space_limit = c(0, 7),
                         separation_type = "Mahalanobis",
                         separation = 4)
plot(stream1)



set.seed(1000)
stream2 <- DSD_Gaussians(k = 5, d = 2, variance_limit = 0.2,
                         space_limit = c(0, 15),
                         separation_type = "Mahalanobis",
                         separation = 10)
plot(stream2)
