library(glmnet)
library(survival)
library(party)
library(MASS)

data3$cox_AIC<-0
data3$cox_BIC<-0


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
 #######put in model here!
  #cox model to put in the for loop 
  model_cox <- coxph(Surv(Survdiag, deathflag) ~ sex+age+eth_other+eth_asian+eth_black+eth_white
                     +charlson_score+centre_volume+imd_quintile, data = trainingData, method = "breslow") 
  
  stepAIC(model_cox, direction=c("backward")) #AIC
  stepAIC(model_cox, k=log(161)) #BIC
  anova(model_cox)
  
  model_AIC <- coxph(Surv(Survdiag, deathflag) ~ age+eth_asian+eth_white+charlson_score+centre_volume+imd_quintile, data = trainingData, method = "breslow")
  model_BIC <- coxph(Surv(Survdiag, deathflag) ~ age+eth_white+charlson_score+centre_volume+imd_quintile, data = trainingData, method = "breslow")
  
  prediction_AIC<-predict(model_AIC,newdata = data.frame(testData))
  prediction_BIC<-predict(model_BIC,newdata = data.frame(testData))
  
  for (j in 0:2666){
    data3$cox_AIC[3*j+i]<-prediction_AIC[j+1]
    data3$cox_BIC[3*j+i]<-prediction_BIC[j+1]
  }
  data3$cox_AIC[8002]<-prediction_AIC[2668]
  data3$cox_BIC[8002]<-prediction_BIC[2668]
  

}
#######forloop ends here

survConcordance(Surv(Survdiag, deathflag)~cox_BIC, data= data3)
survConcordance(Surv(Survdiag, deathflag)~cox_AIC, data= data3)





