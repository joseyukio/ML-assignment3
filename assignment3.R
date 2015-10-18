## Author: José Yukio Akazawa

library(plyr)
library(dplyr)
library(jpeg)
library(cluster)

# ### This part shall be executed only once.
# 
# path_bic <- "data_bic"
# 
# ## Read the filenames that contains the descriptors. Only .txt files will be read.
# filenames <- list.files(path_bic, pattern = ".txt", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# 
# ## Function to read each file descriptor, separate per column (fixed width) and
# ## add the file name as column
# read_bic_descriptors <- function(filename){
#         ret <- read.fwf(file = filename, skip = 1, widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
#         ret$Source <- filename # Add filename as column
#         ret
# }
# 
# ## Set working directory
# setwd(path_bic)
# 
# ## Store the dataframe with the descriptors
# bic.dataframe <- ldply(filenames, read_bic_descriptors)
# 
# ## Write in R format. Only done once.
# saveRDS(bic.dataframe, file = "bic_dataframe.rds")
# 
# ## Restore working directory
# setwd("..")
###############################################################################

## Read the bic dataframe
bic.dataframe <- readRDS("data_bic/bic_dataframe.rds")
## Change the name of the files descriptors to match the figures file names
bic.dataframe[,129] <- sub("txt", "jpg",bic.dataframe[,129])
## Associate the figure file names as rowname
rownames(bic.dataframe) <- bic.dataframe[,129]

## Remove the last colunm (file name) and the first one (contains only 9's)
mydata <- bic.dataframe[,2:128]

###############################################################################
## Find K using elbow method. First column is not considered here
nbr <- seq(1,81432, 10)
mydata.wss <- mydata[nbr,]

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
## start for k
start.K <- 1000
## end for k
end.K = 20000

## Change the steps for iteration. Steps of 100.
for (i in seq(start.K,end.K,1000)) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

#for (i in start.K:end.K) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:end.K, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

###############################################################################
## Use hierarchical clustering to help finding K. Not used in the report.

d <- dist(as.matrix(mydata))
hc <- hclust(d)
plot(hc)

###############################################################################

## Kmeans
#bic.kmeans <- kmeans(bic.dataframe, 100, nstart = 25)
## Run kmeans. Do not use the last column 129 "Source"
k=8000
system.time(bic.kmeans <- kmeans(mydata, k, iter.max = 1000))

## Check the output
bic.kmeans$cluster
bic.kmeans$centers
bic.kmeans$totss
bic.kmeans$tot.withinss
bic.kmeans$withinss
bic.kmeans$betweenss
bic.kmeans$size
bic.kmeans$iter
bic.kmeans$ifault

# Check the centroids
##e.g. for cluster 30
bic.kmeans$centers[30,]

## Check the distribution
plot(table(sort(bic.kmeans$size)), col = 1:10, ylab = "# clusters", xlab = "size of cluster", main = "k = 8000")

## 
bic.kmeans$size
## Check the sizes and respective indexes
sort(tmp$size)
sort.int(tmp$size, index.return = TRUE)

## We can check the a specific cluster. e.g. the largest/smallest ones
## Save the clustered images in different folders.
## E.g. Cluster 1
i=6944
filenames <- bic.kmeans$cluster[bic.kmeans$cluster==i]
filenames <- as.data.frame(filenames)
filenames <- rownames(filenames)
mydir <- paste0("cluster", i)
dir.create(mydir)
file.copy(paste0("data/",filenames), mydir)
######################


## We can check a range of clusters
## i = index of the cluster
start_i <- 1
end_i <- 50
for (i in (start_i:end_i)) {
        filenames <- bic.kmeans$cluster[bic.kmeans$cluster==i]
        filenames <- as.data.frame(filenames)
        filenames <- rownames(filenames)
        mydir <- paste0("cluster", i)
        dir.create(mydir)
        file.copy(paste0("data/",filenames), mydir)
}



# 
# Analyze the centroids of some groups.Do they make sense when you 
# map back the features into images? Analyze their closest neighbors
#in the groups. Do they make sense? Are they talking about the same
# type of images? 

## Check the centers/centroids and map back to the image file names. 
## Notice that the centroids can be a non-existent point in the original dataset.
## This happens due to the calculations done on the distances.
## I selected the centroid by looking a specific folder and getting a existing point very similar to the centroid.

# ## Plot the image. Just if you want to see it inside R.
# image1.n <- readJPEG("data/ff93affcd626307bc2b7b169113f8831.jpg", TRUE)
# plot(1:2, type='n')
# rasterImage(image1, 1.2, 1.27, 1.8, 1.73)
# rasterImage(image1.n, 1.5, 1.5, 1.9, 1.8)

#######################################################################
# Not used.
# fitted.bic <- fitted(bic.kmeans)
# resid.bic <- mydata - fitted.bic
# resid.bic <- bic.dataframe[,2:128] - fitted.bic

## Create the graph
## Set png device
#png("plot1.png", width = 2048, height = 2048, units = "px")

# mydata.scaled <- cmdscale(mydata)
# 
# plot(mydata[,1:2], col = bic.kmeans$cluster)
# 
# points(bic.kmeans$centers, col = 1:k, pch = 8)
# 
# plot(mydata[,1:2], type = 'n')
# text(mydata, labels=bic.kmeans$cluster, col=bic.kmeans$cluster)

## Close the png device
#dev.off()

#######################################################################
## PCA

## Calculate the PCA. Scale is not needed.
pca.bic <- prcomp(mydata)
# The plot method returns a plot of the variances (y-axis) associated
# with the PCs (x-axis). The Figure below is useful to decide how many
# PCs to retain for further analysis. 
plot(pca.bic, type = "l", main = "PC x Variances", col = 2)
## According to the graph 6 components would be enough. This accounts
## for 0.61907 (cumulative proportion)
## Run k-means using only the first 6 components.

## Calculate the k for 6 components
#nbr <- seq(1,81432, 10)
# nbr <- 81432
# mydata.wss <- mydata[nbr,]
# 
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# ## start for k
# start.K <- 1000
# ## end for k
# end.K = 20000
# 
# ## Change the steps for iteraction. Steps of 100.
# for (i in seq(start.K,end.K,1000)) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
# 
# #for (i in start.K:end.K) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
# 
# plot(1:end.K, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

## If I try to use k=8000 I got this:
## Error in kmeans(mydata[, 1:6], k, nstart = 3, iter.max = 100) : 
## more cluster centers than distinct data points.
## So I decreased k

## Calculate PCA (45) and k = 4000.
k=40000
system.time(bic.kmeans <- kmeans(mydata[,1:45], k, iter.max = 100))

#biplot(pca.bic,) # not used

## Check the cumulative proportion
summary(pca.bic)
## According to the summary 45 components accounts for > 95% of the
## variance of the data.
## Repeat k-means using the best case without PCA (k=8000)
k=8000
system.time(bic.kmeans <- kmeans(mydata[,1:45], k, nstart = 3, iter.max = 100))
