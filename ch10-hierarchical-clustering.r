# Hierarchical Clustering (agglomerative)

# simulated example
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# dist creates the euclidean distance matrix
# method is the linkage method (complete, single, average, centroid)
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

# plot the dendrograms
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", ylab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", ylab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", ylab = "", sub = "", cex = 0.9)

# use cutree to view cluster labels
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# complete and average do pretty well, but single only separates one observation
cutree(hc.single, 4)

# scaling variables before hierarchical clustering
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features (Complete Linkage)")

# correlation based distance can be computed using as.dist()
# this only makes sense for data with at least three features
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", ylab = "")
