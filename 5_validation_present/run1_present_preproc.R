# 2021 11 02 I.Zliobaite
# on present day

data_AF <- read.csv('occurence_IUCN_AF.csv', header = TRUE, sep = ",")

sp_names <- colnames(data_AF)

n_sites <- 350

gen_names <- c()
for (sk in 2:length(sp_names)){
  sp_now <- sp_names[sk]
  #gen_now <- strsplit(sp_now,'\\.')[[1]][1]
  gen_now <- sp_now
  gen_names <- c(gen_names,gen_now)
}

un_gen <- unique(gen_names)

no_species <- apply(data_AF[,2:dim(data_AF)[2]],1,sum)
ind <- which(no_species>=3)
data_AF <- data_AF[ind,]

set.seed(1891)

take_sites <- sample(dim(data_AF)[1],n_sites)

data_occ <- matrix(0, n_sites, length(un_gen))
colnames(data_occ) <- un_gen

for (sk in 1:length(gen_names)){
  for (sk2 in 1:n_sites){
    if (data_AF[take_sites[sk2],sk+1]==1){
      data_occ[sk2,gen_names[sk]] <- 1
    }
  }
  
}

no_occur <- apply(data_occ,2,sum)
ind <- which(no_occur>=3)
data_occ <- data_occ[,ind]

write.table(data_occ, file = "data_occ_AF.csv",col.names = TRUE,row.names = FALSE, sep = '\t')  

library(gplots)
pdf('plot_occ_AF.pdf',width = 20, height = 50)
palf <- colorRampPalette(c("white","black"))
heatmap.2(data_occ,col = palf,density.info="none",trace="none",dendrogram = "none", Rowv = FALSE, Colv = FALSE,margins=c(7,12))
#heatmap.2(data_occ,col = palf,density.info="none",trace="none",dendrogram = "none", Rowv = TRUE, Colv = TRUE,margins=c(7,12))
dev.off()



