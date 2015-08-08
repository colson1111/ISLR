
#  Performing PRINCIPLE COMPONENTS ANALYSIS (PCA) on the USArrests data set (base R)

# get row names
states = row.names(USArrests)
states

# view column names
names(USArrests)

# variable means
apply(USArrests, 2, mean)

# variable variance
apply(USArrests, 2, var)

# mean and variance show need to standardize the data before PCA

# PERFORM PCA WITH SCALING
pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)

# the center and scale components correspond to the means and standard deviations of the variables
pr.out$center

pr.out$scale

# rotation matrix provides the principal component loadings (each column provides the corresponding principal component loading vector)
pr.out$rotation

# There are four distinct principal components.

# The x matrix has the principal component score vectors in its columns
pr.out$x

# plot first two principal components
# scale = 0:  ensures the arrows are scaled to represent the loadings
biplot(pr.out, scale = 0)

# reproduce figure from chapter (PCs can have different signs)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

# standard deviation of each principal component
pr.out$sdev

# variance explained by each principal component
pr.var <- pr.out$sdev ^ 2
pr.var

# proportion of variance explained by each principal component
pve <- pr.var/sum(pr.var)
pve
# The first principal component explains 62.00% of the variation in the data

# plot the pve explained by each component (and cumulative PVE)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal COmponent", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')



