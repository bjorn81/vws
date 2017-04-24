library(corrplot)
library(NbClust)

# For more infor check:
# http://r-marketing.r-forge.r-project.org/Instructor/Intro%20Factor%20Analysis/intro-factor-analysis.pdf

####################################################
## Example data analysis, clustering observatsion ##
####################################################



# i) load data
data(mtcars) # for csv files, use: mydata <- read.csv("/folder/subfolder/datafile.csv", stringsasfactors = FALSE)
# Remove variable "am" (automatic gearbox)
mydata <- mtcars[, -9]
head(mydata)

# ii) check for outliers in data
d2 <- outlier(mydata)
sat.d2 <- data.frame(mydata,d2)
pairs.panels(sat.d2,bg=c("green3","mediumpurple3")[(d2 > 15)+1],pch=21, hist.col = "gray55", col = "red2")
# Remove rows with chi-square > 25
mydata <- mydata[chsq.scores <= 25, ] #none ins sample data set

# iii) scale variables
mydata.scaled <- scale(mydata, scale = TRUE, center = TRUE)

############################
#  2 Examine correlations  #
############################

# iiii) Examine correlations:
corrplot(cor(mtcars), col = colorRampPalette(c("mediumpurple4","white","green4"))(256), 
         tl.col = "red4", mar = c(1.1, 0, 2.1, 0)) # adjust third value to make room above for long names


############################
#  3 Clustering  #
############################

# a) Get an idea of how many clusters the data should be divided into
nc <- NbClust(scale(mydata, center = TRUE, scale = TRUE), distance = "euclidean", method = "average",
              min.nc = 2, max.nc = 4)

# b) Perform clustering

# i) Perform hierachiacal clustering

# a) Hiearachical clustering
d.mat <- dist(mydata.scaled)
hc <- hclust(d.mat, method = "ward.D2") #method can also be "average", "single", "complete", "mcquitty"
plot(hc)

# ii) Re-examine how many groups is reasonable to segment into with this method

#Get assignments by every possible cut in height
hc.assigned.groups <- cutree(hc, h =hc$height) #Can also specify height to cut tree at
# Show how the number of clusters decreases with higher cutting height
nclusts <- cbind(as.numeric(colnames(hc.assigned.groups)), 
                 apply(hc.assigned.groups, 2, function(x) length(unique(x))))
plot(nclusts, type = "o", xlab = "cutting height", ylab = "number of clusters", 
     pch = 19, col = "red3", main = "Cutting height and number of clusters", cex = 0.75)
grid()

# Go with 2 groups and see how the separate in first two principal components
k <- 3

# iii) do analysis with selected number of clusters
groups.hc <- cutree(hc, k = k)
table(groups.hc)
groups.km <- kmeans(mydata.scaled, centers = 3)


# iiii) Plot the resulting clusters in 2D by principal components analysis 
fit <- princomp(mydata, cor=TRUE)
fit <- fa(mydata.scaled, nfactors=3, rotate = "varimax")

# Plot the two components, see if automatic and manual cars separate well
plot(fit$scores[,1:2], type = "n", main = "Hierachical")
text(fit$scores[,1:2], labels = row.names(mydata), cex = 0.5, col = rainbow(k)[groups.hc])

plot(fit$scores[,1:2], type = "n", main = "K-means")
text(fit$scores[,1:2], labels = row.names(mydata), cex = 0.5, col = rainbow(k)[groups.km$cluster])

############################
#  4 Get descriptives  #
############################

# create function for descriptive stats (needs to be run only once)
means.by.clust <- function(data, cluster, df = FALSE){
  tot.mean <- apply(data, 2, mean)
  seg.means <- t(aggregate(data, by = list(cluster), FUN = mean, na.rm = TRUE))
  seg.means <- seg.means[2:nrow(seg.means),]
  seg.means <- seg.means / tot.mean
  seg.means <- round(seg.means, 2)
  colnames(seg.means) <- paste0("Segment", 1:(ncol(seg.means)))
  if (df == TRUE) data <- as.data.frame(data)
  return(seg.means)
}

descriptives <- means.by.clust(data = mydata, cluster = groups.hc, df = FALSE)
# descriptives <- means.by.clust(data = mtcars, cluster = k2.f.assigned.groups, df = FALSE) 

hmcols<-colorRampPalette(c("white","blue4"))(256)

desc.heat <- heatmap(descriptives, Rowv=NA, Colv=NA, 
                     col = hmcols, scale="column", margins=c(5,3), cexCol = 1, cexRow = 1, main = "Relative values")
