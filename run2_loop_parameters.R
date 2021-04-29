# 2019 07 18 I.Zliobaite  
# test for implicit recommenders
# factorization andauc_test cross-validation predictrions
# 2020 05 01 North America

data_all <- read.csv('3_data/data_occ.csv', header = TRUE, sep = "\t")
occ_neg <- read.csv('3_data/data_negatives.csv', header = TRUE, sep = "\t")

n_users <- dim(data_all)[1]
n_columns <- dim(data_all)[2]
n_items <- n_columns - 10

do_auc_cv <- TRUE

alpha_now <- 30
n_features_now <- 10
lambda_now <- 10
cik_now <- 10

n_remove_test <- 10
cik_cv <- 10

set.seed(1981)


results_all <- c()

for (alpha_now in c(5,10,20,30)){
  print(alpha_now)
  for (n_features_now in c(5,10,15,20)){
    for (lambda_now  in c(5,10,15,20)){

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
auc_now <- round(auc_test,digits = 4)

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
cv_mean_predictions <- round(apply(res_cv_raw,2,mean),digits = 3)
cv_sd_predictions <- round(apply(res_cv_raw,2,sd),digits = 3)
cv_auc <- round(mean(res_cv_auc),digits = 3)
cv_auc_sd <- round(sd(res_cv_auc),digits = 3)


mean_data_now <- round(mean(data_now),digits = 3)
mean_predictions_now <- round(mean(REC),digits = 3)
mean_predictions_pos <- round(mean(REC[ind_true1]),digits = 3)
mean_predictions_neg <- round(mean(REC[ind_true0]),digits = 3)
animals_predict <- apply(round(REC),2,sum)
animals_true <- apply(round(data_now),2,sum)
mean_error_animals <- round(mean(abs(animals_predict-animals_true)),digits = 3)
localities_predict <- apply(round(REC),1,sum)
localities_true <- apply(round(data_now),1,sum)
mean_error_localities <- round(mean(abs(localities_predict-localities_true)),digits = 3)
correlation_everything <- round(cor(as.vector(REC),as.vector(data_now)),digits = 3)
err <- REC-data_now
mean_absolute_error <- round(mean(abs(err)),digits = 3)



results_all <- rbind(results_all,cbind(alpha_now,n_features_now,lambda_now,cik_now,n_remove_test,cik_cv,auc_now,cv_mean_predictions[1],cv_sd_predictions[1],cv_mean_predictions[2],cv_sd_predictions[2],cv_auc,cv_auc_sd,mean_predictions_now,mean_data_now,mean_error_animals,mean_error_localities,correlation_everything,mean_absolute_error))
write.table(results_all, file = "4_outputs1/results_loop.csv",col.names = TRUE,row.names = FALSE, sep = '\t')      


    }
  }
}
