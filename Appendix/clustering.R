library(mvtnorm)


# As the variance of the components shrinks the decision boundary
# approaches the k means decision boundary
pdf('~/GoogleDrive/PhD/Thesis/figures/gmm-variance.pdf')
#curve(exp(-x**2/.1),from=0,to=1)
mu <- c(0, 2)
set.seed(1234)
x <- c(rnorm(n=10,mean=mu[1]), rnorm(n=10,mean=mu[2]))
xlim <- range(x)
f <- function(x,sd=1) dnorm(x,mean=mu[1], sd=sd)/(dnorm(x,mean=mu[1],sd=sd)+dnorm(x,mean=mu[2],sd=sd))
curve(f(x), from=xlim[1], to=xlim[2], col='white', xlab='x', ylab='probability of belonging to first component')
points(cbind(x, 0), pch=20) 
sds <- c(1,.5,.1)
for (i  in 1:length(sds)) curve(f(x,sd=sds[i]), from=xlim[1], to=xlim[2], add=TRUE, lty=i)
rp <- vector('expression',length(sds))
for (i in 1:length(sds)) rp[[i]] <- substitute(expression(sigma == sd),list(sd=sds[[i]]))[[2]]
legend('topright', legend=rp, lty=1:length(sds), bty='n')
dev.off()

# The total within-cluster SSQ decreases with K.
#
library(car)
library(mclust)

plot.chulls <- function(data, classification, col='black') {
  for (k in sort(unique(classification))) {
    x <- data[which(classification==k),]
    p <- x[chull(x),]
    p <- rbind(p, p)
    lines(p, lwd=2)
  }
}



### Clean easy separable clusters
#package:mclust
#A dataset consisting of 1000 observations drawn from a
#14-component normal mixture in which the covariances of the
#components have the same size and shape but differin orientation.
data(wreath)
plot(wreath, pch=20)

### Easy
noise <- .05
set.seed(1234)
n <- 2000
l <- list(
          list(tau=.1, mu=c(0,0), sigma=noise*diag(2)),
          list(tau=.2, mu=c(1,1), sigma=noise*diag(2)),
          list(tau=.1, mu=c(0,2), sigma=noise*diag(2)),
          list(tau=.1, mu=c(2,0), sigma=noise*diag(2)),
          list(tau=.5, mu=c(2,2), sigma=noise*diag(2))
          )
d <- as.matrix( cbind(mvtnorm::rmvnorm(n*l[[1]]$tau, mean=l[[1]]$mu, sigma=l[[1]]$sigma), 1) )
for (i in 2:length(l)) d <- rbind(d, cbind(mvtnorm::rmvnorm(n*l[[i]]$tau, mean=l[[i]]$mu, sigma=l[[i]]$sigma), i))
d.easy <- data.frame(x=d[,1], y=d[,2], label=factor(d[,3]))
X.easy <- d.easy[,1:2]
plot(X.easy, pch=20)

### Hard
noise <- .5
set.seed(1234)
n <- 2000
l <- list(
          list(tau=.1, mu=c(0,0), sigma=noise*diag(2)),
          list(tau=.2, mu=c(1,1), sigma=noise*diag(2)),
          list(tau=.1, mu=c(0,2), sigma=noise*diag(2)),
          list(tau=.1, mu=c(2,0), sigma=noise*diag(2)),
          list(tau=.5, mu=c(2,2), sigma=noise*diag(2))
          )
d <- as.matrix( cbind(mvtnorm::rmvnorm(n*l[[1]]$tau, mean=l[[1]]$mu, sigma=l[[1]]$sigma), 1) )
for (i in 2:length(l)) d <- rbind(d, cbind(mvtnorm::rmvnorm(n*l[[i]]$tau, mean=l[[i]]$mu, sigma=l[[i]]$sigma), i))
d.hard <- data.frame(x=d[,1], y=d[,2], label=factor(d[,3]))
X.hard <- d.hard[,1:2]
plot(X.hard, pch=20)


K.max <- 10
kmeans.res.easy <- lapply(1:K.max, function(k) kmeans(X.easy,k,nstart=n))
kmeans.res.hard <- lapply(1:K.max, function(k) kmeans(X.hard,k,nstart=n))


# the variance ratio criterion: \citet{Calinski:1974bt}
# When the sd is increased 10-fold we overestimate the number of clusters to 7 instead of 5.
pdf('~/GoogleDrive/PhD/Thesis/figures/variance-ratio-criterion.pdf')
ch <- function(x) (x$betweenss/(length(x$centers)-1))/(x$tot.withinss/(length(x$cluster)-length(x$centers)))
figure.labels <- iter(paste(letters,')',sep=''))
par(mfrow=c(2,2))
#
ch.easy <- sapply(kmeans.res.easy, ch)
ch.max <- which.max(ch.easy)
plot(1:K.max, ch.easy, col=ifelse(1:K.max==ch.max,'red','black'), xlab='Number of clusters', ylab='Variance Ratio Criterion')
lines(1:K.max, ch.easy)
title(nextElem(figure.labels), adj=0)
plot(X.easy, col=kmeans.res.easy[[ch.max]]$cluster, pch=kmeans.res.easy[[ch.max]]$cluster, xlab='', ylab='')
title(nextElem(figure.labels), adj=0)
plot.chulls(X.easy, classification=kmeans.res.easy[[ch.max]]$cluster)
#
ch.hard <- sapply(kmeans.res.hard, ch)
ch.max <- which.max(ch.hard)
plot(1:K.max, ch.hard, col=ifelse(1:K.max==ch.max,'red','black'), xlab='Number of clusters', ylab='Variance Ratio Criterion')
lines(1:K.max, ch.hard)
title(nextElem(figure.labels), adj=0)
plot(X.hard, col=kmeans.res.hard[[ch.max]]$cluster, pch=kmeans.res.hard[[ch.max]]$cluster, xlab='', ylab='')
title(nextElem(figure.labels), adj=0)
plot.chulls(X.hard, classification=kmeans.res.hard[[ch.max]]$cluster)
dev.off()



#
plot(1:K.max, sapply(kmeans.res, function(x) (x$betweenss)),pch=20)
points(1:K.max, sapply(kmeans.res, function(x) (x$tot.withinss)),pch=21)

res <- Mclust(wreath, G=1:20)
plot(res)





#voronoi partitions
library(deldir)
centers <- kmeans.res[[14]]$centers
dd <- deldir(centers[,1],centers[,2])
plot(dd,wlines='tess')
points(wreath, pch=20)


#source('~nikolas/bin/FCS/fcs.R')
#plotClusters(wreath, classification=kmeans.res[[14]]$cluster, ellipses=FALSE)


