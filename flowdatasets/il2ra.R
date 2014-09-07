library(ggplot2)
source('~nikolas/bin/FCS/fcs.R')

par(cex.lab=1.5, cex.main=2)

fcsFiles <- unique(unlist(lapply(strsplit(list.files(pattern='cad.*.fcs', path='~nikolas/dunwich/Projects/IL2RA/FCS.Gated/Calli/all.gated'), '-'), function(x) x[[1]])))
naive.cd25pos.name <- 'lymphocytes-cd4-cd127hicd25pos-cd45rapos.fcs'
d <- data.frame()
for (fcsFile in fcsFiles) { 
    date <- getDate(paste(file.path('~nikolas/dunwich/Projects/IL2RA/FCS.Gated/Calli/all.gated',fcsFile), naive.cd25pos.name, sep='-'))
    d <- rbind(d,data.frame(fcsFile=fcsFile, date=date))
}


read.csv('~nikolas/Projects/IL2RA/Calli_CD25bright_CBR200.csv')->x
length(table(x$individual))

x$fcsFile <- tolower(x$fcsFile)
print(dim(x <- merge(x, d, by='fcsFile')))
x$fcsFile <- gsub('.fcs','',x$fcsFile)
x$date <- x$date.y



pdf('~nikolas/GoogleDrive/PhD/Thesis/flowdatasets/figures/il2ra-samples-time.pdf',width=10,height=5)
qplot(as.Date(x$date),binwidth=1,xlab='',ylab='number of samples processed on day')
dev.off()

#which(table(x$individual)>1)

# repeated individuals
x$date <- as.Date(x$date)
x <- x[order(x$date),]
d1 <- x[duplicated(x$individual,fromLast=TRUE),c('individual','date')]
colnames(d1) <- c('individual','day1')
d2 <- x[duplicated(x$individual,fromLast=FALSE),c('individual','date')]
colnames(d2) <- c('individual','day2') 
d <- merge(d1,d2) 
d$`day diff` <- as.numeric(d$day2-d$day1)
d <- merge(d, unique(read.csv('~nikolas/Projects/IL2RA/CellPhenotypes/recalled.individuals.pch')[,c('individual','pch')]))
write.csv(d[,c('individual','pch','day1','day2','day diff')],quote=FALSE)

