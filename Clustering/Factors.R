library(nFactors)
library(psych)
library(corrplot)
library(NbClust)

# For more infor check:
# http://r-marketing.r-forge.r-project.org/Instructor/Intro%20Factor%20Analysis/intro-factor-analysis.pdf

########################################################
## Example data analysis, factorisaion and clustering ##
########################################################

#
# Maximally capture the correlations among the original variables
# (after accounting for error)
# 2 Each factor is associated clearly with a subset of the variables
# 3 Each variable is associated clearly with (ideally) only one factor
# 4 The factors are maximally differentiated from one another

###########################
#  1 load and clean data  #
###########################

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
pairs(mtcars) # For small datasets
corrplot(cor(mtcars), col = colorRampPalette(c("mediumpurple4","white","green4"))(256), 
         tl.col = "red4", mar = c(1.1, 0, 2.1, 0)) # adjust third value to make room above for long names

###################################
#  3 Determine number of factors  #
###################################

ev <- eigen(cor(mydata)) # get eigenvalues, ceneral rule: keep factors with eigen values > 1
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) #Three out of four statistics suggests 2 factors

#################
#  4 Fit model  #
#################

mtcars.fa <- fa(mydata.scaled, nfactors=2, rotate = "varimax") # varimax means orthogonal (unncorrelated) factors
print(mtcars.fa$loadings, digits=2, cutoff=.2, sort=TRUE)

# Some visualisation of factor loadings
plot(mtcars.fa$loadings,type="n", main = "Factor loadings") # set up plot 
text(mtcars.fa$loadings,labels=names(mydata),cex=.7) # add variable names
fa.diagram(mtcars.fa) # Shows only largest loadings, little bit false picture of what's going on

#################
#  5 Evaluate  #
#################


class(fit2$loadings)
# plot factor 1 by factor 2 
load <- fit2$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names
scree.plot(fit$correlation)
plot(fit2$scores, col = c("red", "blue")[mtcars$am + 1], pch = 19)




# perform principal components analysis on dataset
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for 
# Plot the two components, see if automatic and manual cars separate well
plot(fit$scores[,1:2], col = c("red", "blue")[mtcars$am + 1], pch = 19)
# Print loadings
loadings(fit) # pc loadings 
plot(fit,type="lines", main = "scree plot") # scree plot to help select number of components
head(fit$scores) # the principal components
biplot(fit) # Description of factor loadings

## Factor analysis ##

# Package nFactors provides statistics for chosing the number of factors
# install.packages("nFactors")

ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) #Three out of four statistics suggests 2 factors

# Create Varimax Rotated Principal Components 
# retaining 5 components 

fit <- principal(mydata, nfactors=2, rotate="varimax")
fit # print results

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit2 <- factanal(mydata, factors = 2, rotation="promax", scores = "regression")
fit2 <- fa(mydata, nfactors = 2, rotation="promax", scores = "regression")
summary(fit2)
print(fit2, digits=2, cutoff=.2, sort=TRUE)
class(fit2$loadings)
# plot factor 1 by factor 2 
load <- fit2$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names
scree.plot(fit$correlation)
plot(fit2$scores, col = c("red", "blue")[mtcars$am + 1], pch = 19)


k2.f <- kmeans(fit2$scores, 2)
plot(fit2$scores, col = k2.f$cluster, pch = 19)
k2.f.assigned.groups <- k2.f$cluster
table(k2.f.assigned.groups)

k2 <- kmeans(mydata, 2)
plot(fit2$scores, col = k2$cluster, pch = 19)

# Hierachiacal clustering

#
d.mat <- dist(scale(mydata, center = TRUE, scale = TRUE))
hc <- hclust(d.mat, method = "ward.D2") #method can also be "average", "single", "complete", "mcquitty"
plot(hc)
hc.assigned.groups <- cutree(hc, h =hc$height) #Can also specify height to cut tree at

# Show how the number of clusters decreases with higher cutting height
nclusts <- cbind(as.numeric(colnames(hc.assigned.groups)), 
                            apply(hc.assigned.groups, 2, function(x) length(unique(x))))
plot(nclusts, type = "o", xlab = "cutting height", ylab = "number of clusters", 
     pch = 19, col = "red3", main = "Cutting height and number of clusters", cex = 0.75)
grid()
colnames(hc.assigned.groups)
table(hc.assigned.groups[1,])

# create function for descriptive stats (needs to be run only once)
# means.by.clust <- function(data, cluster, df = FALSE){
#   tot.mean <- apply(data, 2, mean)
#   seg.means <- t(aggregate(data, by = list(cluster), FUN = mean, na.rm = TRUE))
#   seg.means <- seg.means[2:nrow(seg.means),]
#   seg.means <- seg.means / tot.mean
#   seg.means <- round(seg.means, 2)
#   colnames(seg.means) <- paste0("Segment", 1:(ncol(seg.means)))
#   if (df == TRUE) data <- as.data.frame(data)
#   return(seg.means)
# }

# How many clusters are there really?
# install.packages("NbClust")

nc <- NbClust(scale(mydata, center = TRUE, scale = TRUE), distance = "euclidean", method = "average",
              min.nc = 2, max.nc = 4)

descriptives <- means.by.clust(data = mtcars, cluster = hc.assigned.groups, df = FALSE)
# descriptives <- means.by.clust(data = mtcars, cluster = k2.f.assigned.groups, df = FALSE) 

hmcols<-colorRampPalette(c("white","blue4"))(256)

desc.heat <- heatmap(descriptives, Rowv=NA, Colv=NA, 
                     col = hmcols, scale="column", margins=c(5,3), cexCol = 1, cexRow = 1, main = "Relative values")
