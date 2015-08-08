# unsupervised techniques using real data

library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

# examine the cancer types for the cell lines
nci.labs[1:4]

table(nci.labs)

# PRINCIPAL COMPONENTS ANALYSIS
# Scale data and perform PCA
pr.out <- prcomp(nci.data, scale = TRUE)

# Function to assign a color to each element of the numeric vector 
# Each cancer type will get the same color
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# Plot the principal component score vectors
par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")

# In general, cell lines corresponding to a single cancer type do tend
# to have similar values on the first few principal componenet score vectors

# get a summary the proportion of variance explained (PVE) by the first few Principal Components:
summary(pr.out)
plot(pr.out)

# plot PVE and cumulative PVE
pve <- 100 * pr.out$sdev ^ 2 / sum(pr.out$sdev ^ 2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

# After about 7 principal components, there is an 'elbow' in the plot
# this suggests it might not be beneficial to examine more than 7 PCs


# CLUSTERING THE OBSERVATIONS OF THE NCI60 DATA
# Do the observations cluster into distinct types of cancer?

# scale the data
sd.data <- scale(nci.data)

# hierarchial clustering with complete, single, and average linkage
par(mfrow = c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", ylab = "", sub = "")

# Go with complete linkage

# Cut the dendrogram at the height that will yield 4 clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

# Look at dendrogram with the cut
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# Print the output of hclust
hc.out

# How do these results compare to k-means clustering with K = 4?
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# the clusters obtained are different.  Cluster 2 in k means is the same as cluster 3 in hierarchical, but the rest are different


# Instead of hierarchical clustering everything, try clustering the first 5 principal components
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = "Hier. Clust. on First Five PC Score Vectors")
table(cutree(hc.out, 4), nci.labs)

# Sometimes, performing clustering on principal components is better than clustering the full data
# We can view the principal components step as 'denoising the data'







