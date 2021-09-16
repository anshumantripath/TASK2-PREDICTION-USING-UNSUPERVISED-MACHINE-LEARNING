# IMPORTING LIBRARIES REQUIRED FOR ANALYSIS

library(tidyverse)
library(cluster)
library(factoextra)


# IMPORTING THE IRIS  DATA SET PRESENT IN MY DIRECTORY IN THE CSV FILE FORMAT 

data = read.csv(file.choose(), header=T)
data
summary(data)
names(data)
data1 = data[,-1]
data2 = data1[,-5]
data2
dim(data2)
summary(data2)

# let us try to scale and center this dataset

scaled_data = as.matrix(scale(data2))
scaled_data



# Finding the optimum number of clusters for k- means classification. 

# The K-Means algorithm requires the number of clusters to be specified.

# Let us choose random value of cluster numbers and see how the clusters are created.

# Let us start with K = 10.   

# R comes with a built-in kmeans() function.

# Let us apply k -means for K = 10 clusters.

kmm = kmeans(scaled_data,10,nstart= 50 ,iter.max = 15) #we keep number of iter.max=15 to ensure the algorithm converges 
#and nstart=50 to #ensure that atleat 50 random sets are choosen.
kmm

# CONCLUSION 

# we want to increase (between _ss / total_ss ) and as we increase the number of clusters , we see it increasing .
# But we don't want to overfit the data . So the idea is to find such a value of K for which the model is not overfitting 
# and at the same time clusters the data as per the actual distribution. 

# Checking total within - cluster sum of square ( which we want to minimize )
attach(kmm)
tot.withinss

# Checking individual  within - cluster sum of square
withinss

# .............................................................................................................

# Elbow method for computing optimum number of clusters --
#  We will plot the percentage of the variance explained by clusters against the number of clusters. 

# step 1. 

data3 = scaled_data

# function to compute total within - cluster sum of square 
set.seed(123)
wss <- function(k){ kmeans (data3 , k , nstart = 50 )$tot.withinss}

# Compute and plot within - cluster sum of square  for k = 1 to k = 15.

k.values <- 1:15

# Extract within-cluster sum square  for 2-15 clusters -

wss_values <- map_dbl(k.values , wss)

plot(k.values , wss_values,
     type = "b" , pch = 19 ,frame = FALSE , col= "red" ,
     main = " The Elbow Method " ,
     xlab = " Number of Clusters k" ,
     ylab = " Total Within Sum of Square ")

# Conclusion from plot :

# Therefore for k=3 the (between_ss/total_ss) ratio tends to change 
# slowly and remain less changing as compared to other k’s.
# so for this data k=3 should be a good choice for number of clusters.
#----------------------------------------------------------------------------------------------------

# Final Analysis and extracting the results using 3 clusters :

set.seed(123)

final <- kmeans(data3 , 3 , nstart = 50)
print(final)

# Visualizing the result :
# Using the "fviz_cluster"

fviz_cluster(final , data = data3)

# We can extract the cluster and add to our initial datato do some descriptive statistics at the cluster level:

data2 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# -----------------------------------------------------------------------------------------------------------------------
