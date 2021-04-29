# 2019 07 19 I.Zliobaite

dataY <- read.csv('4_outputs1/matY.csv', header = TRUE, sep = "\t")
dataX <- read.csv('4_outputs1/matX.csv', header = TRUE, sep = "\t")
data_animals <- read.csv('4_outputs/gen_summary.csv', header = TRUE, sep = "\t")
data_sites <- read.csv('4_outputs/loc_summary.csv', header = TRUE, sep = "\t")
data_all <- read.csv('3_data/data_occ.csv', header = TRUE, sep = "\t")
pred_all <- read.csv('4_outputs1/predictions_occ_train.csv', header = TRUE, sep = "\t")

mypal <- c('#1B9E77',"#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666",'#33a02c','#fb9a99')
#mypal <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
mypalbw <- colorRampPalette(c("black","white","black"))(100)

n_clust <- 7

age_v <- paste('',round(data_animals[,'max_age'],digits = 0),'-',round(data_animals[,'min_age'],digits = 0),'',sep='')
hyp_v <- paste('H',round(data_animals[,'hyp'],digits=1),sep='')
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',age_v,sep=''))
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',round(data_animals[,'hyp_context_ex'],digits=1),'   ',round(data_animals[,'lop_context_ex'],digits=1),sep=''))
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',round(data_animals[,'hyp_context_ex'],digits=2),'   ',round(data_animals[,'hyp_context_sd_ex'],digits=2),sep=''))
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',round(data_animals[,'hyp_context_ex'],digits=1),sep=''))
labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],'  (',data_animals[,'FAMILY'],')  ',age_v,'   (',round(data_animals[,'hyp_context_ex'],digits=1),')',sep=''))
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',data_animals[,'n_gen'],sep=''))
#labels_gen  <- as.vector(paste('   ',data_animals[,'GENUS'],' (',data_animals[,'FAMILY'],') ',sep=''))
#labels_gen  <- as.vector(data_animals[,'GENUS'])
labels_loc <- as.vector(paste(data_sites[,'NAME'],'(',data_sites[,'COUNTRY'],')',round(data_sites[,'MID_AGE'],digits =1)))

rownames(dataX) <- labels_loc
rownames(dataY) <- labels_gen

n_loc <- dim(data_all)[1]
n_columns <- dim(data_all)[2]
n_gen <- n_columns - 10
n_half_loc <- round(n_loc/2)
data_occ <- as.matrix(data_all[,1:n_gen])
pred_occ <- as.matrix(pred_all[,1:n_gen])

data_occ_bin <- data_occ
ind <- which(data_occ_bin>0)
data_occ_bin[ind] <- 1

colnames(data_occ) <- labels_gen
rownames(data_occ) <- labels_loc 
colnames(pred_occ) <- labels_gen
rownames(pred_occ) <- labels_loc

data_occ_bin <- data_occ
ind <- which(data_occ_bin>0)
data_occ_bin[ind] <- 1


library(ape)

Matrix <- as.matrix(dataY)
sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
sim <- sim %*% t(sim)
dd <- as.dist(1 - sim)

library(corrplot)
library(RColorBrewer)
col1 <- colorRampPalette(brewer.pal(9,"BrBG"))
col2 <- colorRampPalette(brewer.pal(9,"RdBu"))
col3 <- colorRampPalette(brewer.pal(5,"Dark2"))

pdf('4_plots/fig_sim.pdf',width = 23, height = 23)
corrplot(sim, method = "square",order = "hclust",col=col2(20),tl.col="black",tl.cex=2,cl.cex =2)
dev.off()

Matrix <- as.matrix(t(data_occ_bin))
sim2 <- Matrix / sqrt(rowSums(Matrix * Matrix))
sim2 <- sim2 %*% t(sim2)
dd2 <- as.dist(1 - sim2)

pdf('4_plots/fig_sim2.pdf',width = 23, height = 23)
corrplot(sim2, method = "square",order = "hclust",col=col2(20),tl.col="black",tl.cex=2,cl.cex =2)
dev.off()

sim3 <- sim - sim2
dd3 <- as.dist(1 - sim3)
pdf('4_plots/fig_sim3.pdf',width = 23, height = 23)
#corrplot(sim3, method = "square")
corrplot(sim3, method = 'square',col=col2(20),tl.col="black",tl.cex=2,cl.cex =2) #type = "upper"  order = "hclust"
#col=col2(20),
dev.off()



dd <- dist(as.matrix(dataY))  
clusters <- hclust(dd,method = 'ward.D2')
clu_cat <- cutree(clusters,n_clust)
hcd <- as.dendrogram(clusters)
hcp <- as.phylo(clusters)

dd_sites <- dist(as.matrix(dataX))  
clusters_sites <- hclust(dd_sites,method = 'ward.D2')
#clusters_sites <- hclust(dd_sites)
clu_cat_sites <- cutree(clusters_sites,n_clust)
hcp_sites <- as.phylo(clusters_sites)

library(vegan)
library(ade4)
#dd_occ <- dist(as.matrix(t(data_occ_bin)),method='canberra')
#dd_occ <- vegdist(as.matrix(t(data_occ_bin)),method='jaccard',binary = TRUE)
dd_occ <- vegdist(as.matrix(t(data_occ_bin)),method='raup',binary = TRUE)
#dd_occ <- dist.binary(as.matrix(t(data_occ_bin)),method=5)

clusters_occ <- hclust(dd_occ,method = 'ward.D2')
#clusters_occ <- hclust(dd_occ)
clu_cat_occ <- cutree(clusters_occ,n_clust)
hcp_occ <- as.phylo(clusters_occ)

#pdf('4_plots/fig_clusters.pdf',width = 40,height = 20)
#plot(clusters,hang = -1)
#dev.off()

pdf('4_plots/fig_clusters_triangle.pdf',width = 80,height = 30)
plot(hcd, type = "triangle")
dev.off()

pdf('4_plots/fig_clusters_sq.pdf',width = 10,height = 20)
plot(hcp, tip.color = mypal[clu_cat])
dev.off()

pdf('4_plots/fig_clusters_clado.pdf',width = 20,height = 20)
plot(hcp, type = "cladogram",tip.color = mypal[clu_cat])
dev.off()

pdf('4_plots/fig_clusters_unroot.pdf',width = 15,height = 15)
plot(hcp, type = "unrooted", no.margin = TRUE,lab4ut="axial",cex = 0.7,tip.color = mypal[clu_cat])
dev.off()

pdf('4_plots/fig_clusters_sites_clado.pdf',width = 10,height = 40)
plot(hcp_sites, type = "cladogram",tip.color = mypal[clu_cat_sites])
dev.off()

pdf('4_plots/fig_clusters_occ_clado.pdf',width = 20,height = 25)
plot(hcp_occ, type = "cladogram",tip.color = mypal[clu_cat_occ])
dev.off()

pdf('4_plots/fig_clusters_occ_sq.pdf',width = 15,height = 15)
plot(hcp_occ,tip.color = mypal[clu_cat_occ])
dev.off()

#corrplot
library(corrplot)

pred_plot <- pred_occ
for (sk in 1:dim(pred_plot)[1]){
  ind_now <- which(pred_plot[sk,]>1)
  pred_plot[sk,ind_now] <- 1
  ind_now <- which(pred_plot[sk,]<0)
  pred_plot[sk,ind_now] <- 0
}

true_plot <- data_occ
for (sk in 1:dim(true_plot)[1]){
  ind_now <- which(true_plot[sk,]>1)
  true_plot[sk,ind_now] <- 1
  ind_now <- which(true_plot[sk,]<0)
  true_plot[sk,ind_now] <- 0
}

#ht <- 25
#wt <- 25

#ind_first_half <- 1:n_half_loc
#ind_second_half <- (n_half_loc+1):n_loc

#pdf('4_plots/fig_predicitons_1.pdf',width = wt,height = ht)
#corrplot(pred_plot[ind_first_half,], method = "color",is.corr = FALSE,cl.lim = c(0,1),col=mypalbw)
#dev.off()

#pdf('4_plots/fig_predicitons_2.pdf',width = wt,height = ht)
#corrplot(pred_plot[ind_second_half,], method = "color",is.corr = FALSE,cl.lim = c(0,1),col=mypalbw)
#dev.off()

#pdf('4_plots/fig_true_1.pdf',width = wt,height = ht)
#corrplot(true_plot[ind_first_half,], method = "color",is.corr = FALSE,cl.lim = c(0,1),col=mypalbw)
#dev.off()

#pdf('4_plots/fig_true_2.pdf',width = wt,height = ht)
#corrplot(true_plot[ind_second_half,], method = "color",is.corr = FALSE,cl.lim = c(0,1),col=mypalbw)
#dev.off()




