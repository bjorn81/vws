library(MASS)      # for MST
# install.packages("vegan")
library(vegan)     # for MST
library(RColorBrewer)
#
data(mtcars) # for csv files, use: mydata <- read.csv("/folder/subfolder/datafile.csv", stringsasfactors = FALSE)
# Remove variable "am" (automatic gearbox)
mydata <- mtcars[, -9]
coeffs <- mydata
coeffs$nps <- NULL
# coeffs <- cor(coeffs)
coeffs <- abs(coeffs)     # need converting into absolute values, as the technique does not like negative values
coeffs.d <- 1-cor(coeffs)   # convert to distance
coeffs.scal <- cmdscale(coeffs.d,k=6,eig=T)
coeffs.scal
#pairs(coeffs)
coeffs.d

tr <- spantree(coeffs.d)
plot(tr, cmdscale(coeffs.d), type = "t")   ## Add tree to a metric scaling 

par(bg = "white")
o <- rank(tr$labels)
plot(tr, type = "t", xlab = "Dimension 1", ylab = "Dimension 2", 
     col = brewer.pal(4,"Set3")[o], fill = "gray30",
     main = "Nearness in space", cex = 1)   ## Find a new configuration to display the tree neatly
