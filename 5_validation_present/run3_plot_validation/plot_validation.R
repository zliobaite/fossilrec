# 2021 12 03 I.Zliobaite
# plot 

data_av <- read.csv('res_present_avg.csv', header = TRUE, sep = "\t")
data_sd <- read.csv('res_present_std.csv', header = TRUE, sep = "\t")

ind_re <- which(data_av[,'rando']==0)
ind_ra <- which(data_av[,'rando']==1)

pdf('fig_real_occurences.pdf',width = 4.5, height = 4.5)
plot(data_av[ind_re,'no'], data_av[ind_re,'pos'], ylim=range(c(0,1)),pch=19, xlab="Missing species", 
     ylab="Estimated probability of occurence", main="Original occurrences",cex = 2,xaxt = "n")
points(data_av[ind_re,'no'], data_av[ind_re,'neg'], ylim=range(c(0,1)),pch=1,cex = 2)
arrows(data_av[ind_re,'no'], data_av[ind_re,'pos']-data_sd[ind_re,'pos'], data_av[ind_re,'no'], data_av[ind_re,'pos']+data_sd[ind_re,'pos'], length=0.05, angle=90, code=3, lwd=1.5, col='grey70')
arrows(data_av[ind_re,'no'], data_av[ind_re,'neg']-data_sd[ind_re,'pos'], data_av[ind_re,'no'], data_av[ind_re,'neg']+data_sd[ind_re,'pos'], length=0.05, angle=90, code=3, lwd=1.5, col='grey70')
axis(1, at=1:5, labels=c('0.1%','1%','10%','50%','90%'))
dev.off()

pdf('fig_rand_occurences.pdf',width = 4.5, height = 4.5)
plot(data_av[ind_ra,'no'], data_av[ind_ra,'pos'], ylim=range(c(0,1)),pch=19, xlab="Missing species", 
     ylab="Estimated probability of occurence", main="Randomized occurences",cex = 2,xaxt = "n")
points(data_av[ind_ra,'no'], data_av[ind_ra,'neg'], ylim=range(c(0,1)),pch=1,cex = 2)
arrows(data_av[ind_ra,'no'], data_av[ind_ra,'pos']-data_sd[ind_ra,'pos'], data_av[ind_ra,'no'], data_av[ind_ra,'pos']+data_sd[ind_ra,'pos'], length=0.05, angle=90, code=3, lwd=1.5, col='grey70')
arrows(data_av[ind_ra,'no'], data_av[ind_ra,'neg']-data_sd[ind_ra,'pos'], data_av[ind_ra,'no'], data_av[ind_ra,'neg']+data_sd[ind_ra,'pos'], length=0.05, angle=90, code=3, lwd=1.5, col='grey70')
axis(1, at=1:5, labels=c('0.1%','1%','10%','50%','90%'))
dev.off()