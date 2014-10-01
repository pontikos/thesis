source('~nikolas/bin/FCS/fcs.R')
source('~nikolas/bin/FCS/normalise-functions.R')
set.seed(1234)


d0 <- density(x0 <- rnorm(10000, mean=1, sd=1))
d1 <- density(x1 <- rnorm(1000, mean=5, sd=3))

#\includegraphics[scale=.5]{figures/normalisation-scaled-a.pdf} 
pdf('figures/normalisation-scaled-a.pdf')
plot(d0, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
lines(d1, lty=2)
dev.off()
#\includegraphics[scale=.5]{figures/normalisation-scaled-b.pdf} 
pdf('figures/normalisation-scaled-b.pdf')
d0 <- density(x0 <- scale(x0))
d1 <- density(x1 <- scale(x1))
plot(d0, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
lines(d1, lty=2) 
dev.off()


d0 <- density(x0 <- rgamma(10000, shape=20, rate=1.5))
d1 <- density(x1 <- rgamma(1000, shape=10, rate=2))
#\includegraphics[scale=.5]{figures/normalisation-quantile-a.pdf} 
pdf('figures/normalisation-quantile-a.pdf')
quantiles <- c(0, .25,.5,.75, 1)
plot(d0, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
lines(d1, lty=2)
qx0 <- quantile(x0,quantiles)
points(qx0,returny(d0,qx0),pch=20) 
qx1 <- quantile(x1,quantiles)
points(qx1, returny(d1,qx1), pch=20)
dev.off()
#\includegraphics[scale=.5]{figures/normalisation-quantile-b.pdf} 
pdf('figures/normalisation-quantile-b.pdf')
d1<-density(x1 <- quantile.normalize(x0, quantiles))
plot(d0, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
qx0 <- quantile(x0,quantiles)
points(qx0,returny(d0,qx0),pch=20) 
lines(d1, lty=2)
qx1 <- quantile(x1,quantiles)
points(qx1, returny(d1,qx1), pch=20)
dev.off()


#same donor pre and after dose
channels <- c('CD25','CD4','CD45RA','FSCA','SSCA')
donor1.pre <- getChannels(read.FCS('data/dilt1d-donor1-eff-pre.fcs',channels),channels)
donor1.post <- getChannels(read.FCS('data/dilt1d-donor1-eff-post.fcs',channels),channels)

n<-'FSCA'
d0<-density(x0 <- donor1.pre[,n])
d1<-density(x1 <- donor1.post[,n])

#\includegraphics[scale=.5]{figures/normalisation-peaks-a.pdf} 
pdf('figures/normalisation-peaks-a.pdf')
plot(d0, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
lines(d1, lty=2)
print(extract.landmarks(x0,max.lms=3)->p0)
points(p0$lms,p0$dens,pch=20)
print(extract.landmarks(x1,max.lms=3)->p1)
points(p1$lms,p1$dens,pch=20)
dev.off()
#\includegraphics[scale=.5]{figures/normalisation-peaks-b.pdf} 
pdf('figures/normalisation-peaks-b.pdf')
f <- function(x)  {
  m <- lm(p0$lms ~ p1$lms)
  cbind(1,x)%*%coefficients(m)
}
d1<-density(x1<-f(x1))
plot(d0, xlim=range(c(d0$x,d1$x)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
points(p0$lms,p0$dens,pch=20)
lines(d1, lty=2)
points(f(p1$lms),returny(d1,f(p1$lms)),pch=20)
dev.off()



#\includegraphics[scale=.5]{figures/normalisation-peaks-a.pdf} 
pdf('figures/normalisation-features-a.pdf')

featureSignif(x0)->f0
featureSignif(x1)->f1
plot(f0$fhat$x.grid[[1]], f0$fhat$est, xlim=range(c(x0,x1)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='', type='l')
lines(f1$fhat$x.grid[[1]], f1$fhat$est, lty=2)
points(f0$fhat$x.grid[[1]][f0$curv], f0$fhat$est[f0$curv], pch=20)
points(f1$fhat$x.grid[[1]][f1$curv], f1$fhat$est[f1$curv], pch=20)

dev.off()
#\includegraphics[scale=.5]{figures/normalisation-peaks-b.pdf} 
pdf('figures/normalisation-features-b.pdf')
f <- function(x)  {
  m <- lm(p0$lms ~ p1$lms)
  cbind(1,x)%*%coefficients(m)
}
d1<-density(x1<-f(x1))
plot(d0, xlim=range(c(d0$x,d1$x)), ylim=range(c(d0$y,d1$y)), main='', xlab='', ylab='')
points(p0$lms,p0$dens,pch=20)
lines(d1, lty=2)
points(f(p1$lms),returny(d1,f(p1$lms)),pch=20)
dev.off()





plot(density(scale(x0)), xlim=range(c(scale(x0),scale(x1))), ylim=range(c(density(scale(x0))$y,density(scale(x1))$y)), main='', xlab='', ylab='')
lines(density(scale(x1)), lty=2) 

channels <- c('PSTAT5','CD4','CD45RA','FSCA','SSCA')
unstim <- getChannels(read.FCS('data/CB00010K_0U_2012-11-13.fcs',channels),channels)
stim <- getChannels(read.FCS('data/CB00010K_1000U_2012-11-13.fcs',channels),channels)
x0 <- unstim[,channels[[2]]]
x1 <- stim[,channels[[2]]]
plot(density(x0), xlim=range(c(x0,x1)), ylim=range(c(density(x0)$y,density(x1)$y)), main='', xlab='', ylab='')
lines(density(x1), lty=2) 


unstim.day1 <- getChannels(read.FCS('data/CB00366X_0U_2012-11-07.fcs',channels),channels)
unstim.day2 <- getChannels(read.FCS('data/CB00366X_0U_2013-03-27.fcs',channels),channels)
x0 <- unstim.day1[,channels[[5]]]
x1 <- unstim.day2[,channels[[5]]]
plot(density(x0), xlim=range(c(x0,x1)), ylim=range(c(density(x0)$y,density(x1)$y)), main='', xlab='', ylab='')
lines(density(x1), lty=2) 


plot(density(scale(x0)), xlim=range(c(scale(x0),scale(x1))), ylim=range(c(density(scale(x0))$y,density(scale(x1))$y)), main='', xlab='', ylab='')
lines(density(scale(x1)), lty=2) 


plot(density(x0), xlim=range(c(x0,x1)), ylim=range(c(density(x0)$y,density(x1)$y)), main='', xlab='', ylab='')
lines(density(quantile.normalize(x0, x1)), lty=2)

lines(density(x1.trans), lty=2)


plot(density(x0), xlim=range(c(x0,x1)), ylim=range(c(density(x0)$y,density(x1)$y)), main='', xlab='', ylab='')
lines(density(gaussNorm.normalize2(x0, x1,3)), lty=2)


plot(density(x0), xlim=range(c(x0,x1)), ylim=range(c(density(x0)$y,density(x1)$y)), main='', xlab='', ylab='')
lines(density(peak.normalize(x0, x1,3)), lty=2)

library(mclust)
res0 <- Mclust(x0,3)
res1 <- Mclust(x1,3)



%\section{Normalisation using Univariate Peak Alignment}
%\begin{figure}[h]
%
%#simulated data
%library(mixtools)
%source('~nikolas/bin/FCS/normalise-functions.R')
%bw <- 0.5
%x <- seq(-3, 11, length.out=3000)
%x0 <- mixtools::rnormmix(1000, mu=c(1,6), lambda=c(.3,.7))
%x1 <- mixtools::rnormmix(1000, mu=.9*c(1,6)+2, lambda=c(.5,.5))
%d0 <- density(x0,bw=bw)
%d0.f <- splinefun(d0$x, d0$y)
%d1 <- density(x1,bw=bw)
%d1.f <- splinefun(d1$x, d1$y)
%#peaks identified with pam
%p0 <- peaks(x0, k=2)
%p1 <- peaks(x1, k=2)
%m <- lm(p0 ~ p1)
%x1.norm <- cbind(1,x1)%*%coefficients(m)
%d1.norm <- density(x1.norm,bw=bw)
%d1.norm.f <- splinefun(d1.norm$x, d1.norm$y)
%p1.norm <- peaks(x1.norm, k=2)
%#before align
%pdf('~nikolas/GoogleDrive/PhD/Thesis/IL2/figures/simulation-peak-align.pdf')
%plot(x, d0.f(x), xlim=c(-3,11), main='', xlab='', col='black', type='l')
%points(p0, d0.f(p0), pch=20, cex=2)
%lines(x, d1.f(x), col='blue', lwd=1.5)
%points(p1, d1.f(p1), pch=20, cex=2, col='blue')
%polygon(c(d0$x,rev(d1$x)),c(d0$y,rev(d1$y)), density=10, angle=90, col='blue', lwd=.5)
%#after peak align
%lines(x, d1.norm.f(x), col='red', lwd=1.5)
%points(p1.norm, d1.norm.f(p1.norm), pch=20, cex=2, col='red')
%polygon(c(x,rev(x)),c(d0.f(x),rev(d1.norm.f(x))),density=10, angle=90, col='red',lwd=.5)
%lines(x, d0.f(x), col='black', lwd=1.5)
%abc <- function(f1, f2, x=seq(-3, 11, length.out=2000)) sum(abs(f1(x)-f2(x)))
%legend('topleft', legend=c(round(abc(d0.f, d1.f)), round(abc(d0.f, d1.norm.f))), fill=c('blue','red'), angle=90, density=20)
%dev.off()
%
%\centering
%\includegraphics[scale=.75]{IL2/figures/simulation-peak-align.pdf}
%\caption{  \label{figure:simulation-peak-align}  In solid black line represents the density function obtained from $5,000$ draws from a mixture of two normal distribution
%with means $\mu_0=(1,6)$ and mixing proportions $\tau_0=(.3,.7)$.  The dashed blue line represents the density function obtained from $5,000$ draws from a mixture of two normal distribution
%where $\mu_1 = 0.9 \mu_0 + 2$ and $\tau_1=(.5,.5)$.  The red dashed line represents the transformation using peak alignment. The red and black distribution are now better aligned. }
%\end{figure}



%%e <- lapply(flat.rep.fcs[names(flat.rep.fcs)[1:4]], function(x) ecdf(lgcl(getChannels(x, 'pstat5'))))
%%d <- lapply(flat.rep.fcs[names(flat.rep.fcs)[1:4]], function(x) density(lgcl(getChannels(x, 'pstat5')),bw=.1))
%%d.f <- lapply(d, function(d) splinefun(d$x,d$y))
%%y.max <- max(sapply(d, function(d) d$y)) 
%%e <- lapply(lymph[names(lymph)[1:4]], function(x) ecdf(lgcl(getChannels(x, 'pstat5'))))
%%d <- lapply(lymph[names(lymph)[1:4]], function(x) density(lgcl(getChannels(x, 'pstat5')),bw=.1))
%%d.f <- lapply(d, function(d) splinefun(d$x,d$y))
%%y.max <- max(sapply(d, function(d) d$y)) 
%%#pdf('~nikolas/lymph-dose-effect.pdf',width=10,height=5)
%%pdf('~nikolas/ungated-dose-effect.pdf',width=10,height=5)
%%par(mfrow=c(1,2))
%%x <- seq(-.5, 3, length.out=20000)
%%plot(x, e[[1]](x), col='white', xlab='pSTAT5', xlim=c(-.5,3), ylab='') 
%%mapply(function(e,lwd,lty) lines(x,e(x),lwd=lwd,lty=lty),e,seq(1,2.5,.5),c(1,2,2,1))
%%legend('topleft',doses, lwd=seq(1,2.5,.5), lty=c(1,2,2,1))
%%plot(x, d.f[[1]](x), col='white', xlab='pSTAT5', xlim=c(-.5,3),ylim=c(0,1),ylab='') 
%%mapply(function(d,lwd,lty) lines(x,d(x),lwd=lwd,lty=lty),d.f,seq(1,2.5,.5),c(1,2,2,1))
%%legend('topleft',doses, lwd=seq(1,2.5,.5), lty=c(1,2,2,1))
%%dev.off() 
%%pdf('~nikolas/ungated-lymph-dose-effect.pdf')
%%par(mfrow=c(1,1))
%%x <- seq(-.5, 3, length.out=20000)
%%plot(x, d.f[[1]](x), col='white', xlab='pSTAT5', xlim=c(-.5,3),ylim=c(0,1),ylab='') 
%%d <- lapply(flat.rep.fcs[names(flat.rep.fcs)[c(1,4)]], function(x) density(lgcl(getChannels(x, 'pstat5')),bw=.1))
%%d.f <- lapply(d, function(d) splinefun(d$x,d$y))
%%y.max <- max(sapply(d, function(d) d$y))
%%mapply(function(d,lwd,lty) lines(x,d(x),lwd=lwd,lty=lty),d.f,c(1,2.5),1)
%%d <- lapply(lymph[names(lymph)[c(1,4)]], function(x) density(lgcl(getChannels(x, 'pstat5')),bw=.1))
%%d.f <- lapply(d, function(d) splinefun(d$x,d$y))
%%y.max <- max(sapply(d, function(d) d$y))
%%#r <- mapply(function(a,b) length(a)/length(b), lymph[1:4], flat.rep.fcs[1:4])
%%mapply(function(d,lwd,lty) lines(x,d(x),lwd=lwd,lty=lty,col='red'),d.f,c(1,2.5),1) 
%%legend('topleft',doses[c(1,4)], lwd=c(1,2.5), lty=1)
%%dev.off()
%%lymph <- sapply(flat.rep.fcs, function(x) { cd4 <- lgcl(getChannels(x, 'cd4')); return(x[2 <  cd4 & cd4 < 2.75,]) })
%%


## Question: do peaks move with gating?
## a) Look at CD45RA in ungated data
## b) Look at CD45RA in CD4+ lymphocytes
## Use logicleTransform(w=1)
## Answer: yes they do! peaks are not conserved on the channels
## which are not used in the gating
par(mfrow=c(1,1))
print(load('~/dunwich/Projects/IL2/PSTAT5-CD25-CD45RA-CD4-FOXP3/pstat5-join/All/KM00813H_2012-08-03.RData'))
print(load('~/dunwich/Projects/IL2/PSTAT5-CD25-CD45RA-CD4-FOXP3/magnetic-manual-gates2/CLR/KM00813H_2012-08-03.RData'))
#
lgcl <- logicleTransform(w=1)
plot(density(lgcl(fcs.data[,'CD45RA'])))
lines(density(lgcl(fcs.data[as.logical(CLR[,'CD4']),'CD45RA'])),col='red')
lines(density(lgcl(fcs.data[as.logical(CLR[,'Lymphocytes']),'CD45RA'])),col='green')
#
lgcl <- logicleTransform(w=1)
plot(density(lgcl(fcs.data[,'SSCA'])))
lines(density(lgcl(fcs.data[as.logical(CLR[,'CD4']),'SSCA'])),col='red')
lines(density(lgcl(fcs.data[as.logical(CLR[,'Lymphocytes']),'SSCA'])),col='green')


## Question: does K nearest neighbour normalise if one of the channels is noisy?
## Answer: the channel which is left out of the joining will be aligned, provided
## the clusters align on the other dimensions.
library(mvtnorm)
### Easy
noise <- .05
set.seed(1234)
n <- 100
D <- 10
l <- list(
          list(tau=.5, mu=rep(0,D), sigma=noise*diag(D)),
          list(tau=.5, mu=c(1,rep(0,D-1)), sigma=noise*diag(D))
          )
d <- as.matrix( cbind(mvtnorm::rmvnorm(n*l[[1]]$tau, mean=l[[1]]$mu, sigma=l[[1]]$sigma), 1) )
for (i in 2:length(l)) d <- rbind(d, cbind(mvtnorm::rmvnorm(n*l[[i]]$tau, mean=l[[i]]$mu, sigma=l[[i]]$sigma), i))
d.easy <- data.frame(d[,1:D], label=factor(d[,D+1]))
X.easy <- d.easy[,1:D]
X1 <- X.easy[which(d.easy$label==1),]
X2 <- X.easy[which(d.easy$label==2),]

library(RANN)
nn <- RANN::nn2(X2[,-1],query=X1[,-1],k=1)

#
pairs(X.easy, pch=20, col=d.easy$label, lower.panel=NULL, upper.panel=function(x, y, ...) {
  X <- cbind(x,y)
  x1 <- X[which(d.easy$label==1),]
  x2 <- X[which(d.easy$label==2),]
  segments(x1[,1], x1[,2], x2[nn$nn.idx,1], x2[nn$nn.idx,2], lwd=.25)
  points(X, pch=20, col=d.easy$label)
}
)

source('~nikolas/bin/FCS/fcs.R')
plotClusters(X.easy, classification=d.easy$label, chulls=FALSE, ellipses=FALSE , cex=.5)








