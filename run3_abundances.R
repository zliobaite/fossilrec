# 2021 03 10 (MF-> Porvoo day) 
# I.Zliobaite

data_flat <- read.csv('4_outputs1/predictions_flat_train.csv', header = TRUE, sep = "\t")

pdf('4_plots/fig_predicitons_1.pdf',width = 10,height = 6)
plot(NA,NA,xlim = c(0,355),ylim = c(-0.3,1.5))
un_genera <- unique(data_flat[,'GENUS'])
for (sk in 1:length(un_genera)){
  ind <- which(data_flat[,'GENUS']==un_genera[sk])
  pred_now <- data_flat[ind,'prediction']
  pred_now <- pred_now[order(pred_now, decreasing = TRUE)]
  lines(pred_now,lty=1)
}
dev.off()

pdf('4_plots/fig_predicitons_2.pdf',width = 10,height = 6)
plot(NA,NA,xlim = c(0,30),ylim = c(-0.3,1.5))
un_localities <- unique(data_flat[,'SITE'])
for (sk in 1:length(un_genera)){
  ind <- which(data_flat[,'SITE']==un_localities[sk])
  pred_now <- data_flat[ind,'prediction']
  pred_now <- pred_now[order(pred_now, decreasing = TRUE)]
  lines(pred_now,lty=1)
}
dev.off()

ind2 <- which(data_flat[,'true']>0)
pdf('4_plots/fig_predicitons_3.pdf',width = 10,height = 6)
plot(NA,NA,xlim = c(0,105),ylim = c(-0.3,1.5))
un_localities <- unique(data_flat[,'SITE'])
for (sk in 1:length(un_genera)){
  ind <- intersect(ind2,which(data_flat[,'SITE']==un_localities[sk]))
  pred_now <- data_flat[ind,'prediction']
  pred_now <- pred_now[order(pred_now, decreasing = TRUE)]
  lines(pred_now,lty=1)
}
dev.off()



#for (sk in 1:length(un_genera)){
#  ind <- which(data_flat[,'GENUS']==un_genera[sk])
#  pred_now <- data_flat[ind,'prediction']
#}

#p <- ggplot(ToothGrowth, aes(x=dose, y=len)) +   geom_violin()



site_of_interest <- 'Pasalar'
#site_of_interest <- 'Somosaguas Norte'

ind <- which(data_flat[,'SITE'] == site_of_interest)
data_site <- data_flat[ind,]

ind <- which(data_site[,'true']>0)
data_pos <- data_site[ind,]

ind <- which(data_pos[,'prediction']<0)
data_pos[ind,'prediction'] <- 0

sum_sum <- sum(data_pos[,'prediction'])

rab <- round(100*data_pos[,'prediction']/sum_sum,digits=1)

data_pos <- cbind(data_pos,rab)

data_pos <- data_pos[order(rab,decreasing=TRUE),]

write.table(data_pos, file = paste("4_outputs3/res_abundances_",site_of_interest,".csv",sep=''),col.names = TRUE,row.names = FALSE, sep = '\t')      


