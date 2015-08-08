# K-Means Clustering

# simulated example
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4


# perform k-means clustering, k = 2
# nstart = 20:  20 initial cluster assignments
km.out <- kmeans(x, 2, nstart = 20)

# cluster asignments are in km.out$cluster
km.out$cluster

# plot data with color = cluster
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2",
     xlab = "", ylab = "", pch = 20, cex = 2)

# In example, we knew there were two clusters, but we don't always.  Try with K = 3.
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out


# compare nstart = 1 to nstart = 20
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss

km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

# When nstart = 1, the total within cluster SS is higher (we want to minimize this)

# Generally run k-means with a larger value of nstart (20, 50)



