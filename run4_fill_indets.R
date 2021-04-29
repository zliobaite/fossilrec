# 2021 03 10 I.Zliobaite

data_flat <- read.csv('4_outputs1/predictions_flat_train.csv', header = TRUE, sep = "\t")
gen_indets <- read.csv('4_outputs/data_gindet.csv', header = TRUE, sep = "\t")
gen_all <- read.csv('4_outputs/gen_summary.csv', header = TRUE, sep = "\t")
loc_all <- read.csv('4_outputs/loc_summary.csv', header = TRUE, sep = "\t")


#family_of_interest <- 'Giraffidae'
#family_of_interest <- 'Suidae'
family_of_interest <- 'Cervidae'
#family_of_interest <- 'Rhinocerotidae'
#family_of_interest <- 'Bovidae'

ind <- which(gen_indets[,'FAMILY'] == family_of_interest)
sites_now <- as.vector(unique(gen_indets[ind,'NAME']))

ind <- which(gen_all[,'FAMILY'] == family_of_interest)
gen_now <- as.vector(unique(gen_all[ind,'GENUS']))

res_all <- c()
sites_used <- c()
for (sk in 1:length(sites_now)){
  ss_now <- sites_now[sk]
  ind <- which(data_flat[,'SITE']==ss_now)
  if (length(ind)>0){
    dd <- data_flat[ind,]
    sites_used <- c(sites_used,ss_now)
    
    res_temp <- c()
    for (sk2 in 1:length(gen_now)){
      gg_now <- gen_now[sk2]
      ind <- which(dd[,'GENUS']==gg_now)
      
      res_temp <- c(res_temp,round(dd[ind,'prediction'],digits = 2))
    }
    res_all <- rbind(res_all,res_temp)
  }  
}
  

gen_info <- c()
for (sk in 1:length(gen_now)){
  gg_now <- gen_now[sk]
  ind <- which(gen_all[,'GENUS'] == gg_now)
  gen_info <- rbind(gen_info,round(gen_all[ind,c('min_age','max_age','hyp_context_ex')],digits = 1))
  
}

res_all <- rbind(t(gen_info),res_all)

colnames(res_all) <- gen_now

loc_min <- c()
loc_cou <- c()
loc_max <- c()
loc_hyp <- c()
for (sk in 1:length(sites_used)){
  ss_now <- sites_used[sk]
  ind <- which(loc_all[,'NAME'] == ss_now)
  loc_min <- c(loc_min,round(loc_all[ind,'MIN_AGE'],digits = 1))
  loc_max <- c(loc_max,round(loc_all[ind,'MAX_AGE'],digits = 1))
  loc_hyp <- c(loc_hyp,round(loc_all[ind,'mn_HYP'],digits = 1))
  loc_cou <- c(loc_cou,as.vector(loc_all[ind,'COUNTRY']))
}

res_all <- cbind(c('','','',sites_used),c('','','',loc_cou),c('','','',loc_min),c('','','',loc_max),c('','','',loc_hyp),res_all)

write.table(res_all, file = paste("4_outputs3/res_all_",family_of_interest,".csv",sep=''),col.names = TRUE,row.names = FALSE, sep = '\t')      
