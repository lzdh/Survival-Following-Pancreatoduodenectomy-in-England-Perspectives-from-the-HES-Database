library(glmnet)
library(survival)
library(party)
library(MASS)

data3$rfs<-0


data_feature <- cbind(data3$surg_date, data3$sex, data3$AgeAtdiag, data3$charlson_score, data3$imd_quintile,
                      data3$flag, data3$centre_volume, data3$eth_white, data3$eth_mixed, data3$eth_black, 
                      data3$eth_asian, data3$eth_other, data3$deathflag, data3$X90_day_death, data3$Survdiag)
colnames(data_feature) <- c('surg_date','sex','age','charlson_score','imd_quintile','centre_flag',
                            'centre_volume', 'eth_white','eth_mixed','eth_black','eth_asian','eth_other',
                            'deathflag', '90daydeathflag', 'Survdiag')

data_feature <- data.frame(data_feature)

for(i in 1:nrow(data_feature)){
  if (data_feature$Survdiag[i]==0) data_feature$Survdiag[i]=0.0001
}

Nset1<-NULL
Nset2<-NULL
Nset3<-NULL

for(i in 0:2666){
  Nset1<-rbind(Nset1,data_feature[3*i+1,])
  Nset2<-rbind(Nset2,data_feature[3*i+2,])
  Nset3<-rbind(Nset3,data_feature[3*i+3,])
}

Nset1<-rbind(Nset1,data_feature[8002,])


###for loop starts here
for(i in 1:3){
  if (i == 1) trainingData <- rbind(Nset2,Nset3); testData<-Nset1[, -which(names(Nset1) %in% c("surg_date","Survdiag","centre_flag","X90daydeathflag","deathflag"))]
  if (i == 2) trainingData <- rbind(Nset1,Nset3); testData<-Nset2[, -which(names(Nset2) %in% c("surg_date","Survdiag","centre_flag","X90daydeathflag","deathflag"))]
  if (i == 3) trainingData <- rbind(Nset1,Nset2); testData<-Nset3[, -which(names(Nset3) %in% c("surg_date","Survdiag","centre_flag","X90daydeathflag","deathflag"))]
  trainingData<-data.frame(trainingData)
  testData<- data.frame(testData)
  
  model_cforest <- cforest(Surv(Survdiag, deathflag) ~ sex+age+imd_quintile+charlson_score+centre_volume+
                            eth_white+eth_mixed+eth_black+eth_asian+eth_other, data = trainingData, 
                           control = cforest_unbiased(ntree = 300))
  
  predictions<- predict(model_cforest,newdata=testData, OOB=TRUE)
  
  for (j in 0:2666){
    data3$rfs[3*j+i]<-predictions[j+1]
  }
  if(i==1) data3$rfs[8002]<-predictions[2668]
  
}
#######forloop ends here

survConcordance(Surv(Survdiag, deathflag)~rfs, data= data3)



