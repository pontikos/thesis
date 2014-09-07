library(beeswarm)


# the smaller the MAF, the more the noise in the phenotype,
# the harder to find association
maf <- .05
maf <- .1
noise <- 2

probs <- c(maf**2, 2*maf*(1-maf), (1-maf)**2)
N <- 1000

genotypes <- sample(c('aa', 'aA', 'AA'), N, prob=probs, replace=TRUE)

d <- data.frame(rbind(cbind(geno='aa', trait=rnorm(N*probs[1],mean=1,sd=noise)) , cbind(geno='aA', trait=rnorm(N*probs[2],mean=2,sd=noise)) , cbind(geno='AA', trait=rnorm(N*probs[3],mean=3,sd=noise))))
d$trait <- as.numeric(d$trait)

# number of minor alleles 0, 1, 2
d$geno <- factor(d$geno, levels=c('AA','aA','aa'))


boxplot(trait ~ geno, data=d, outline = FALSE)
beeswarm(trait ~ geno, data=d, col = 4, pch = 16, add = TRUE)

m <- lm(trait~geno, data=d)
summary(m)
coeffs <- coef(m)
#abline(a=coeffs[1], b=coeffs[2])

#rmultinom(1000, 3, probs)

# additive model: 2 df, every copy of risk allele increases risk 0, 1, 2

# recessive model: 1 df, 2 copies needed risk allele

# dominant model: 1 copy of risk allele enough

