# 2019 07 18 I.Zliobaite  
# test for implicit recommenders
# factorization and cross-validation predictions
# 2020 05 01 North America

data_all <- read.csv('3_data/data_occ.csv', header = TRUE, sep = "\t")
occ_neg <- read.csv('3_data/data_negatives.csv', header = TRUE, sep = "\t")

n_users <- dim(data_all)[1]
n_columns <- dim(data_all)[2]
n_items <- n_columns - 10

do_flat_predictions <- TRUE
do_auc_cv <- TRUE

alpha_now <- 10
n_features_now <- 10
lambda_now <- 10
cik_now <- 10

n_remove_test <- 10
cik_cv <- 10

set.seed(1981)

data_now <- as.matrix(data_all[,1:n_items])
rownames(data_now) <- paste(round(data_all[,'MID_AGE'],digits = 1),data_all[,'COUNTRY'],data_all[,'NAME']) 
#print('Number of occurrences')
#print(t(apply(data_now,2,sum)))

factorize <- function(data_occ,n_features,alpha,lambda,cik,rec_mat){
  n_sites <- dim(data_occ)[1]
  n_genera <- dim(data_occ)[2]
  C <- 1 + alpha*data_occ
  P <- as.matrix((data_occ > 0) + 0)
  X <- matrix(rnorm(n_sites*n_features), ncol = n_features)
  Y <- matrix(rnorm(n_genera*n_features), ncol = n_features)
  Ix <- diag(n_sites)
  Iy <- diag(n_genera)
  Ilambda <- lambda*diag(n_features)
  for (sk in 1:cik){
    #Y-transpose-Y and X-transpose-X
    xTx <- t(X)%*%X
    yTy <- t(Y)%*%Y
    # Loop through all sites
    for (uu in 1:n_sites){
      Cu <- diag(C[uu,])
      Cu_I <- Cu - Iy
      for_inversion <- yTy + t(Y)%*%Cu_I%*%Y + Ilambda
      X[uu,] <- solve(for_inversion)%*%t(Y)%*%Cu%*%P[uu,]
    }
    # Loop through all genera
    for (ii in 1:n_genera){
      Ci <- diag(C[,ii])
      Ci_I <- Ci - Ix
      for_inversion <- xTx + t(X)%*%Ci_I%*%X + Ilambda
      Y[ii,] <- solve(for_inversion)%*%t(X)%*%Ci%*%P[,ii]
    }
  }
  rec <- X%*%t(Y)
  #print(mean(rec))
  #print(sd(rec))
  #print(max(rec))
  #print(min(rec))
  if (rec_mat){
    write.table(X, file = "4_outputs1/matX.csv",col.names = TRUE,row.names = FALSE, sep = '\t')
    write.table(Y, file = "4_outputs1/matY.csv",col.names = TRUE,row.names = FALSE, sep = '\t')
  }
  return(rec)
}

#full fit on train data
REC <- round(factorize(data_now,n_features_now,alpha_now,lambda_now,cik_now,TRUE),digits = 2)
REC_all <- cbind(REC,data_all[,(n_items+1):n_columns])
colnames(REC_all) <- colnames(data_all)
colnames(REC) <- colnames(data_all)[1:dim(REC)[2]]
rownames(REC) <- paste(round(data_all[,'MID_AGE'],digits = 1),data_all[,'COUNTRY'],data_all[,'NAME']) 

write.table(REC_all, file = "4_outputs1/predictions_occ_train.csv",col.names = TRUE,row.names = FALSE, sep = '\t')


# ROC on training data full
library(pROC)
ind_true1 <- which(data_now>=0.9)
ind_true0 <- which(occ_neg==1)

data_test <- REC[c(ind_true0,ind_true1)]
labels_test <- c(rep(0,length(ind_true0)),rep(1,length(ind_true1)))
data_frame_test <- as.data.frame(cbind(data_test,labels_test))

roc_test <- roc(labels_test,data_test)
auc_test <-  auc(roc_test)
print(paste('AUC train set',auc_test))

#cross-validation
res_cv_raw <- c()
res_cv_auc <- c()

for (sk in 1:cik_cv){

  ind_true_remove <- sample(ind_true1, n_remove_test)
  ind_false_remove <- sample(ind_true0, n_remove_test)

  data_now_test <- data_now
  data_now_test[ind_true_remove] <- 0
  REC_cv <- round(factorize(data_now_test,n_features_now,alpha_now,lambda_now,cik_now,FALSE),digits = 2)
  res_cv_raw <- rbind(res_cv_raw,c(mean(REC_cv[ind_true_remove]),mean(REC_cv[ind_false_remove])))

  if (do_auc_cv){
    data_test <- REC_cv[c(ind_false_remove,ind_true_remove)]
    labels_test <- c(rep(0,length(ind_false_remove)),rep(1,length(ind_true_remove)))
    data_frame_test <- as.data.frame(cbind(data_test,labels_test))
    roc_test <- roc(labels_test,data_test)
    auc_test <-  auc(roc_test)
  }else{
    auc_test <- 0
  }
  res_cv_auc <- c(res_cv_auc,auc_test)
}
print('======Statistics cv======')
print(paste('Cross-v: mean predictions raw',round(apply(res_cv_raw,2,mean),digits = 3)))
print(paste('Cross-v: sd predictions raw',round(apply(res_cv_raw,2,sd),digits = 3)))
print(paste('Cross-v: AUC mean',round(mean(res_cv_auc),digits = 3)))
print(paste('Cross-v: AUC sd',round(sd(res_cv_auc),digits = 3)))

sum_all <-c()
data_sites <- read.csv('4_outputs/loc_summary.csv', header = TRUE, sep = "\t")
for (sk in 1:dim(data_sites)[1]){
  loc_now <- data_sites[sk,'LIDNUM']
  ind_now <- which(REC_all[,'LIDNUM']==loc_now)
  row_now <- REC[ind_now,]
  row_now[row_now<0] <- 0
  sum_all <- c(sum_all,sum(row_now))
}
frac <- sum_all/data_sites[,'n_loc']
data_sites <- cbind(data_sites,sum_all,frac)

write.table(data_sites, file = "4_outputs1/loc_completeness.csv",col.names = TRUE,row.names = FALSE, sep = '\t')  


if (do_flat_predictions){
  
  predictions_flat <- c()
  for (sk in 1:dim(REC)[1]){
    #print(sk)
    for (sk2 in 1:dim(REC)[2]){
      r_now <- data_now[sk,sk2]
      pred_now <- REC[sk,sk2]
      o_neg <- occ_neg[sk,sk2]
      #predictions_flat <- rbind(predictions_flat,cbind(colnames(data_now)[sk2],as.vector(data_all[sk,c('NAME','COUNTRY')]),data_all[sk,c('MID_AGE','LIDNUM','n_gen')],r_now,pred_now))
      predictions_flat <- rbind(predictions_flat,cbind(colnames(data_now)[sk2],as.vector(data_all[sk,'NAME']),as.vector(data_all[sk,'MID_AGE']),as.vector(data_all[sk,'MAX_AGE']),as.vector(data_all[sk,'MIN_AGE']),o_neg,r_now,pred_now))
    }
  }
  
  #colnames(predictions_flat) <- c('GENUS','SITE','COUNTRY','MID_AGE','LIDNUM','n_gen','true','prediction')
  colnames(predictions_flat) <- c('GENUS','SITE','MID_AGE','MAX_AGE','MIN_AGE','negative','true','prediction')
  
  write.table(predictions_flat, file = "4_outputs1/predictions_flat_train.csv",col.names = TRUE,row.names = FALSE, sep = '\t')  
}


print('======Statistics train======')
print(paste('mean predictions',round(mean(REC),digits = 3)))
print(paste('mean predictions positives',round(mean(REC[ind_true1]),digits = 3)))
print(paste('mean predictions true negatives',round(mean(REC[ind_true0]),digits = 3)))

print(paste('mean data',round(mean(data_now),digits = 3)))
animals_predict <- apply(round(REC),2,sum)
animals_true <- apply(round(data_now),2,sum)
print(paste('mean error animals',round(mean(abs(animals_predict-animals_true)),digits = 3)))
localities_predict <- apply(round(REC),1,sum)
localities_true <- apply(round(data_now),1,sum)
print(paste('mean error localities',round(mean(localities_predict-localities_true),digits = 3)))
print(paste('correlation everything',round(cor(as.vector(REC),as.vector(data_now)),digits = 3)))
err <- REC-data_now
print(paste('mean absolute error',round(mean(abs(err)),digits = 3)))
