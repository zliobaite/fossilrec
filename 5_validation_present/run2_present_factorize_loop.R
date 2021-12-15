# 2021 11 02 I.Zliobaite
# for present day

data_all <- as.matrix(read.csv('data_occ_AF.csv', header = TRUE, sep = "\t"))

n_users <- dim(data_all)[1]
n_columns <- dim(data_all)[2]

alpha_now <- 10
n_features_now <- 10
lambda_now <- 10
cik_now <- 10

n_remove_test <- 10
cik_cv <- 10

set.seed(1891)

res_all <- c()
res_avg <- c()
res_std <- c()

n_rand_pos <- sum(data_all)
n_rand_all <- dim(data_all)[1]*dim(data_all)[2]

for (randomized in c(FALSE,TRUE)){
  print(randomized)
  
  #select random amount of occurrences that is the same amount as in the original data
  
  if (randomized){
    data_all <- data_all*0
    ind_rand <- sample(n_rand_all,n_rand_pos)
    data_all[ind_rand] <- 1  
  }
  
  for (n_out in c(5,54,543,2747,4893)){
    print(n_out)
    
    res_avg_temp <- c()
    for (sk in 1:30){
      print(sk)
      
      
      ind_pos <- which(data_all==1)
      n_pos <-length(ind_pos)
      n_train <- n_pos - n_out
      ind_nulify <- ind_pos[sample(n_pos,n_out)]
      data_all_nulified <- data_all
      data_all_nulified[ind_nulify] <- 0
      ind_neg <- which(data_all==0)
      n_neg <-length(ind_neg)
      ind_sanity <- ind_neg[sample(n_neg,n_out)]
      
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
        
        return(rec)
      }
      
      #full fit on train data
      REC <- round(factorize(data_all_nulified,n_features_now,alpha_now,lambda_now,cik_now,TRUE),digits = 2)
      
      REC_plot <- REC
      ind <- which(REC_plot>1)
      REC_plot[ind] <- 1
      ind <- which(REC_plot<0)
      REC_plot[ind] <- 0
      
      res_all <- rbind(res_all,c(randomized,n_train,n_out,mean(REC_plot[ind_nulify]),mean(REC_plot[ind_sanity])))
      res_avg_temp <- rbind(res_avg_temp,c(randomized,n_train,n_out,mean(REC_plot[ind_nulify]),mean(REC_plot[ind_sanity])))
      
    }
    
    res_avg <- rbind(res_avg,apply(res_avg_temp,2,mean))
    res_std <- rbind(res_std,apply(res_avg_temp,2,sd))
    
  }
  
}

write.table(res_all, file = "res_present_all.csv",col.names = TRUE,row.names = FALSE, sep = '\t')
write.table(res_avg, file = "res_present_avg.csv",col.names = TRUE,row.names = FALSE, sep = '\t')
write.table(res_std, file = "res_present_std.csv",col.names = TRUE,row.names = FALSE, sep = '\t')
