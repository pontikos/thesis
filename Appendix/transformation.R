library(ggplot2)
library(gtable)
library(flowCore)
library(scales)
source('~nikolas/bin/FCS/fcs.R')

## This is to show how the choice of transform can influence the modality of the distribution.

#/chiswick/data/store/facs/Tony-FCS/PSTAT5/CD25/CD45RA/CD4/FOXP3/CB00055J_01U_2012-10-03.fcs
f <- '~nikolas/GoogleDrive/PhD/Thesis/data/CB00055J_01U_2012-10-03.fcs'
# CD45RA channel
flowCore::read.FCS(f)@exprs[,12]->x

range(x)
par(mfrow=c(2,2))
plot(density(x))
quant <- quantile(x, .75)
quant2 <- quantile(x, .90)
quant3 <- quantile(x, .99)
abline(v=quant)
abline(v=quant2)
abline(v=quant3)
plot(density(x[which(x < quant)]))
plot(density(x[which(x < quant2)]))
abline(v=quant)
plot(density(x[which(x < quant3)]))


par(mfrow=c(3,1))
from <- min(x)
to <- 100000
print(range(logicleTransform(w=0)(c(from,to))))
curve(logicleTransform(w=0)(x), from=from, to=to, lwd=.5)
#curve(logicleTransform(w=.1)(x), from=from, to=to)
curve(logicleTransform(w=.5)(x), from=from, to=to, add=TRUE, lwd=1)
curve(logicleTransform(w=1)(x), from=from, to=to, add=TRUE, lwd=2)
#curve(logicleTransform(w=1.5)(x), from=from, to=to, add=TRUE, lwd=3)
curve(asinh(x)/4, from=from, to=to, add=TRUE, lwd=3)
curve(log10(x)-1, from=from, to=to, add=TRUE, lwd=3)
plot(density(x[which(x < to)]))
#par(new=TRUE) 
plot(density(logicleTransform(w=1)(x[which(x < to)])))


plotLogicleTransform <- function(x, w=0) {
    fun <- logicleTransform(w=w)
    myd <- data.frame (x=x, yvar=fun(x))
    xlim <- range(myd$x)
    ylim <- range(myd$yvar)
    p1 <- ggplot(myd,aes(x=x)) +  coord_cartesian(xlim, ylim) + theme(legend.position = "none")
    p1 <- p1 + stat_function(fun=fun) + geom_hline(yintercept=w, alpha=.5) + geom_vline(xintercept=0, alpha=.5)
    p2 <- ggplot(myd, aes(x = x)) + stat_density(geom='area') + coord_cartesian(xlim)
    p3 <- ggplot(myd, aes(x = yvar)) + stat_density(geom='area') + coord_flip(ylim)
    gt <- ggplot_gtable(ggplot_build(p1))
    gt2 <- ggplot_gtable(ggplot_build(p2))
    gt3 <- ggplot_gtable(ggplot_build(p3))
    gt1 <- gtable_add_cols(gt, unit(0.3, "null"), pos = -1)
    gt1 <- gtable_add_rows(gt1, unit(0.3, "null"), pos = 0) 
    gt1 <- gtable_add_grob(gt1, gt2$grobs[[which(gt2$layout$name == "panel")]], 1, 4, 1, 4)
    gt1 <- gtable_add_grob(gt1, gt2$grobs[[which(gt2$layout$name == "axis-l")]], 1, 3, 1, 3, clip = "off") 
    gt1 <- gtable_add_grob(gt1, gt3$grobs[[which(gt3$layout$name == "panel")]], 4, 6, 4, 6)
    gt1 <- gtable_add_grob(gt1, gt3$grobs[[which(gt3$layout$name == "axis-b")]], 5, 6, 5, 6, clip = "off")
    print(grid.newpage())
    print(grid.draw(gt1))
}

xvar <- x[which(x < 2000)]
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/logicle-transform-a.pdf')
p1 <- plotLogicleTransform(xvar,w=0) 
dev.off()
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/logicle-transform-b.pdf')
p2 <- plotLogicleTransform(xvar,w=.25)
dev.off()
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/logicle-transform-c.pdf')
p3 <- plotLogicleTransform(xvar,w=.5)
dev.off()
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/logicle-transform-d.pdf')
p4 <- plotLogicleTransform(xvar,w=1)
dev.off()

#does not work: sad
#print(multiplot(p1, p2, p3, p4, cols=2))


f <- function(x) logicleTransform(w=1)(x)
curve(f(x), from=min(x), to=2000)
par(new=TRUE)
plot(density(x[which(x < 2000)]))
par(new=TRUE)
plot(density(f(x[which(x < 2000)])))


#
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/log10-transform.pdf')
y <- x
y[which(y<=0)] <- 1
d1 <- normalised.density(log10(y))
d2 <- normalised.density(log10(x-min(x)+1))
plot(d1, xlim=range(d1$x,d2$x), ylim=range(d1$y,d2$y), col='black', main='', xlab='', ylab='')
lines(d2, col='red')
dev.off()


#w = 0.5, t = 262144, m = 4.5, a = 0

#
pdf('~nikolas/GoogleDrive/PhD/Thesis/Appendix/figures/logicle-transform.pdf')
w<-seq(0,.75,.25)
plot(normalised.density(logicleTransform(w=0)(x)),col=1,main='',xlab='',ylab='')
sapply(w,function(w)lines(normalised.density(logicleTransform(w=w)(x)),col=1+w*4))
legend(x="topleft", legend=paste('w',w,sep='='), col=1+w*4, lty=1, lwd=1)
dev.off()








