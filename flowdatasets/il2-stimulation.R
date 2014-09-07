library(ggplot2)

x <- read.csv('~nikolas/dunwich/Projects/IL2/PSTAT5-CD25-CD45RA-CD4-FOXP3/individual-date.csv',col.names=c('individual','date'))
x$date <- as.Date(x$date)

length(table(x$individual))

pdf('~nikolas/GoogleDrive/PhD/Thesis/flowdatasets/figures/il2-stimulation-samples-time.pdf')
qplot(as.Date(x$date),binwidth=1,xlab='',ylab='number of samples processed on day')
dev.off()



