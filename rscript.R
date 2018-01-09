Median_age <- read.table("W:/Health Informatics_Analytics/Felicity Evison/MSC Projects/Jessie/Median_age.txt", header = TRUE)
data2 <- read.csv("W:/Health Informatics_Analytics/Felicity Evison/MSC Projects/Jessie/Data2.csv", header = TRUE) #resection data without death in hospital

AllOp <- read.csv("W:/Health Informatics_Analytics/Felicity Evison/MSC Projects/Jessie/AllOp.csv", header = TRUE)
#AllOp$op_date <- strptime(AllOp$op_date, '%d/%m/%Y') #Reformat the dates 
#AllOp$op_date <- substr(AllOp$op_date, 1,4)

data3 <- read.csv("W:/Health Informatics_Analytics/Felicity Evison/MSC Projects/Jessie/data3.csv", header = TRUE) #resection data with death in hospital
#data3$surg_date <- strptime(data3$surg_date, '%d/%m/%Y') #Reformat the dates 
#data3$surg_date <- substr(data3$surg_date, 1,4) #Keep only year in surg_date

###############
#change deathflag to 90-days mortality
mortality_90 <- data2

for (i in 1:7999){
  if (mortality_90$Survival.time[i] > 90) mortality_90$deathflag[i]<-0
}
###############

# analysis on years
data2$surg_date <- strptime(data2$surg_date, '%d/%m/%Y') #Reformat the dates 
data_year <- data2
data_year$surg_date <- substr(data_year$surg_date, 1,4) #Keep only year in surg_date

x <- 0
for(i in 1:7999){
  if (data_year$surg_date[i] ==2014 & data_year$deathflag[i]==1) x<-x+1
} #equivalent with *

nrow(subset(data_year, data_year$surg_date==2002)) 
nrow(subset(data_year, data_year$surg_date==2001 & data_year$deathflag==1)) # * calculate death rate

nrow(subset(mortality_90, mortality_90$surg_date$year==114 & mortality_90$deathflag==1)) # 90day mortality in each year, 
# 114 means year of 2014, 101 year of 2001

################################################
# Calculate total number of providers each year & plot
require(graphics)
require(stats)
y <-0   # contains the string of number of centre                                                                        
for (i in 1:14){
  op_year <- subset(AllOp, AllOp$op_date==(2000+i))
  y[i] <- nrow(as.matrix(unique(op_year$Centre)))
} 
x <- c(2001:2014)
op_year<-cbind(x,y)
z<-lm(y~x,data=data.frame(op_year))
plot(op_year, type='b', col='blue', xlab ='Year', ylab = 'Number of Centres' )
abline(z)
################################################



################################################
y <-0   # contains the string of number of consultant                                                                      
for (i in 1:14){
  op_year <- subset(AllOp, AllOp$op_date==(2000+i))
  y[i] <- nrow(as.matrix(unique(op_year$pconsult)))
} 
plot(x,y, type='b', col='blue', xlab ='Year', ylab = 'Number of Surgeons' )
################################################

################################################
# Count how many whipples were done each year
# reformate the data containing only operation year, centre code and consultant code
centre_consultant <- cbind(AllOp$op_date, AllOp$Centre, AllOp$pconsult) 
centre_consultant <- data.frame(centre_consultant)
colnames(centre_consultant) <- c('year', 'centre', 'consultant')

y <-0   # contains the numbers of whipples in total per annum                                                                      
for (i in 1:14){
  op_year <- subset(AllOp, AllOp$op_date==(2000+i))
  y[i] <- nrow(as.matrix(unique(op_year$Patient_ID)))
} 
plot(x,y, type='b', col='blue', xlab ='Year', ylab = 'Number of Operations' )
################################################


################################################
# find the volumn of operations per centre per annum & plot boxplot
library(plyr)
centre_volumn <- ddply(AllOp,.(AllOp$op_date,AllOp$Centre),nrow) # count the number of each duplication

colnames(centre_volumn) <- c('year', 'centre','volumn')

for (i in 1:14){
  subset_year <- subset(centre_volumn, centre_volumn$year==(2000+i))
# qqplot(subset_year$centre, subset_year$volumn, xlab='centre code', ylab = 'centre volumn')
}

qqplot(centre_volumn$centre, centre_volumn$volumn)
boxplot(centre_volumn$volumn ~ centre_volumn$year, xlab = 'Year', ylab = 'Centre Volumn')
################################################


################################################
# find the volumn of operations per surgeon per annum & plot boxplot
library(plyr)
surgeon_volume <- ddply(AllOp,.(AllOp$op_date,AllOp$pconsult),nrow) # count the number of each duplication

colnames(surgeon_volume) <- c('year', 'surgeon','volume')
boxplot(surgeon_volumn$volumn ~ surgeon_volumn$year, xlab = 'Year', ylab = 'Surgeon Volumn')

centre_volumn_sum <- ddply(AllOp,.(AllOp$Centre),nrow)
hist(as.matrix(centre_volumn_sum$V1))

surgeon_volumn_sum <- ddply(AllOp,.(AllOp$pconsult),nrow)
hist(as.matrix(surgeon_volumn_sum$V1))
################################################


#################################################
#90day mortality on UK/Netherland centre quartiles
q1_UK_centre <- subset(centre_volumn, centre_volumn_sum$V1<=3)

y <- 0   # contains the numbers of whipples in total per annum  
no_cen_l <- 0 
no_cen_lm <- 0 
no_cen_mh <- 0 
no_cen_h <- 0 
no_cen_vh <- 0 
no_op_l <- 0 
no_op_lm <- 0 
no_op_mh <- 0 
no_op_h <- 0 
no_op_vh <- 0 
mor_l <- 0 
mor_lm <- 0 
mor_mh <- 0 
mor_h <- 0 
mor_vh <- 0 

for (i in 1:14){
#group the centres by 'low volumn(<=3)', 'low-med(>3,<=11)', 'med-high(>11,<=30)', 'high(>30,<=50)' and 'vhigh(>50)'.
  op_year_l <- subset(centre_volumn, centre_volumn$year==(2000+i) & centre_volumn$volumn <=3)
  op_year_lm <- subset(centre_volumn, centre_volumn$year==(2000+i) & centre_volumn$volumn >3 & centre_volumn$volumn <=11)
  op_year_mh <- subset(centre_volumn, centre_volumn$year==(2000+i) & centre_volumn$volumn >11 & centre_volumn$volumn <=30)
  op_year_h <- subset(centre_volumn, centre_volumn$year==(2000+i) & centre_volumn$volumn >30 & centre_volumn$volumn <=49)
  op_year_vh <- subset(centre_volumn, centre_volumn$year==(2000+i) & centre_volumn$volumn >=50)
#calculate the number of centres in each group  
  no_cen_l[i] <- nrow(op_year_l)
  no_cen_lm[i] <- nrow(op_year_lm)
  no_cen_mh[i] <- nrow(op_year_mh)
  no_cen_h[i] <- nrow(op_year_h)
  no_cen_vh[i] <- nrow(op_year_vh)
#calculate the number of surgeries in each centre
  no_op_l[i] <- sum(op_year_l$volumn)
  no_op_lm[i] <- sum(op_year_lm$volumn)
  no_op_mh[i] <- sum(op_year_mh$volumn)
  no_op_h[i] <- sum(op_year_h$volumn)
  no_op_vh[i] <- sum(op_year_vh$volumn)
#take the subset from AllOp table of each year and different volumn of groups
  mor_sub_l <- AllOp[AllOp$Centre %in% op_year_l$centre & AllOp$op_date %in% op_year_l$year,]
  mor_sub_lm <- AllOp[AllOp$Centre %in% op_year_lm$centre & AllOp$op_date %in% op_year_lm$year,]
  mor_sub_mh <- AllOp[AllOp$Centre %in% op_year_mh$centre & AllOp$op_date %in% op_year_mh$year,]
  mor_sub_h <- AllOp[AllOp$Centre %in% op_year_h$centre & AllOp$op_date %in% op_year_h$year,]
  mor_sub_vh <- AllOp[AllOp$Centre %in% op_year_vh$centre & AllOp$op_date %in% op_year_vh$year,]
#calculate 90 day mortality
  mor_l[i] <- sum(mor_sub_l$death_90_days)
  mor_lm[i] <- sum(mor_sub_lm$death_90_days)
  mor_mh[i] <- sum(mor_sub_mh$death_90_days)
  mor_h[i] <- sum(mor_sub_h$death_90_days)
  mor_vh[i] <- sum(mor_sub_vh$death_90_days)
} 
UK_no_cen <- cbind(a$year ,no_cen_l,no_cen_lm,no_cen_mh, no_cen_h, no_cen_vh)
UK_no_op <- cbind(a$year ,no_op_l,no_op_lm,no_op_mh, no_op_h, no_op_vh)
UK_mor <- cbind(a$year ,mor_l,mor_lm,mor_mh, mor_h, mor_vh)
colnames(UK_no_op) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')
colnames(UK_no_cen) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')
colnames(UK_mor) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')

NE_no_cen <- cbind(a$year ,no_cen_l,no_cen_lm,no_cen_mh, no_cen_h, no_cen_vh)
NE_no_op <- cbind(a$year ,no_op_l,no_op_lm,no_op_mh, no_op_h, no_op_vh)
NE_mor <- cbind(a$year ,mor_l,mor_lm,mor_mh, mor_h, mor_vh)
colnames(NE_no_op) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')
colnames(NE_no_cen) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')
colnames(NE_mor) <- c('year', 'Low','Low-Med','Med-High','High','VHigh')

mor_sub_lm <- AllOp[AllOp$Centre %in% op_year_lm$centre & AllOp$op_date %in% op_year_lm$year,]

########### ploting ############
library(ggplot2)
library(reshape2)
df <- melt(UK_no_op, id.vars = 'year')
df1 <- melt(NE_no_op, id.vars = 'year')
ggplot(df, aes(year,value, col=variable)) +  geom_point()

ggplot(df, aes(year,value,group=1)) +  geom_point()+stat_smooth() + facet_wrap(~variable)

par(mfrow=c(2,2))
plot(df[1:14,1],df[1:14,3],xlab='Year', ylab='Number of Surgeries', main="Low")
plot(df[15:28,1],df[15:28,3],xlab='Year', ylab='Number of Surgeries',main="Low-Med")
plot(df[29:42,1],df[29:42,3],xlab='Year', ylab='Number of Surgeries',main="Med-High")
plot(df[43:56,1],df[43:56,3],xlab='Year', ylab='Number of Surgeries',main="High")
plot(df[57:70,1],df[57:70,3],xlab='Year', ylab='Number of Surgeries',main="Very High")

par(mfrow=c(2,3))
plot(df1[1:14,1],df1[1:14,3],xlab='Year', ylab='Number of Surgeries',main="Low")
plot(df1[15:28,1],df1[15:28,3],xlab='Year', ylab='Number of Surgeries',main="Low-Med")
plot(df1[29:42,1],df1[29:42,3],xlab='Year', ylab='Number of Surgeries',main="Med-High")
plot(df1[43:56,1],df1[43:56,3],xlab='Year', ylab='Number of Surgeries',main="High")
plot(df1[57:70,1],df1[57:70,3],xlab='Year', ylab='Number of Surgeries',main="Very High")
########### ploting end ###########



########################################################################################################
#Analysis on gender of Pancreatic Cancer

#plot KM curves
#overall survival
km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$sex, data = data3)

survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$sex, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
chiSquared   <- pchisq(survivalDiff$chisq, 1, lower.tail=FALSE) # second entry is number of clusters-1

par(mar=c(5,5,2,3))
plot(km, xlab = "Days", ylab = "Survival Probability", col = c( "blue","red", "chocolate", "darkgreen"),
     lwd = 1.5, font = 2, cex.lab = 1.7, cex.axis = 1.7)#, xmax = 90, ymin = 0.9)
legend("topright", c("Male", "Female"), lty = 1,lwd= 4,
       col = c( "blue", "red", "chocolate", "darkgreen"), bty = "n", text.font=2,cex=1.5)
titleString = paste("Survival probability on gender", 
                    " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
title(titleString)

#90-day survival
mortality_90 <- data3

for (i in 1:nrow(data3)){
  if (mortality_90$Survdiag[i] > 90) mortality_90$deathflag[i]<-0
}

km <- survfit(Surv(data3$Survdiag, data3$X90_day_death == 1) ~ data3$sex, data = data3)

survivalDiff <- survdiff(Surv(data3$Survdiag, data3$X90_day_death == 1) ~ data3$sex, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
chiSquared   <- pchisq(survivalDiff$chisq, 1, lower.tail=FALSE) # second entry is number of clusters-1

par(mar=c(5,5,2,3))
plot(km, xlab = "Days", ylab = "Survival Probability", col = c( "blue","red", "chocolate", "darkgreen"),
     lwd = 2, font = 2, cex.lab = 1.7, cex.axis = 1.7, xmax = 90, ymin = 0.9)
legend("topright", c("Male", "Female"), lty = 1,lwd= 4,
       col = c("blue","red",  "chocolate", "darkgreen"), bty = "n", text.font=2,cex=1.5)
titleString = paste("90-day survival probability on gender", 
                    " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
title(titleString)

#####################################################################################




########################################################################################################
#Analysis on age groups of Pancreatic Cancer
data3$age_flag <- 0
#Overall survival
for (i in 1:nrow(data3)){
  if (data3$AgeAtdiag[i] < 59) data3$age_flag[i]=1
  if (data3$AgeAtdiag[i] >= 59 &  data3$AgeAtdiag<66) data3$age_flag[i]=2
  if (data3$AgeAtdiag[i] >= 66 & data3$AgeAtdiag < 72) data3$age_flag[i]=3
  if (data3$AgeAtdiag[i] >= 72) data3$age_flag[i]=4
}

#plot KM curves
#overall survival
  km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$age_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$age_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 3, lower.tail=FALSE) # second entry is number of clusters-1

   par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2)#, xmax = 90, ymin = 0.9)
  legend("topright", c("Age group [18,59)", "Age group [59,66)", "Age group [66,72)", "Age group [72,88]"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Survival probability on age groups", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)

#90-day survival
  mortality_90 <- data3
  
  for (i in 1:nrow(data3)){
    if (mortality_90$Survdiag[i] > 90) mortality_90$deathflag[i]<-0
  }
  km <- survfit(Surv(data3$Survdiag, data3$X90_day_death == 1) ~ data3$age_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$X90_day_death == 1) ~ data3$age_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 3, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2, xmax = 90, ymin = 0.86)
  legend("bottomleft", c("Age group [18,59)", "Age group [59,66)", "Age group [66,72)", "Age group [72,88]"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Survival probability on age groups", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  

  
#Each year survial
for (i in 1:14){
  #group the centres by 'low volumn(<=3)', 'low-med(>3,<=11)', 'med-high(>11,<=30)', 'high(>30,<=50)' and 'vhigh(>50)'.
  age1 <- subset(data2, data2$year==(2000+i) & data2$AgeAtdiag < 59)
  age2 <- subset(data2, data2$year==(2000+i) & data2$AgeAtdiag >= 59 &  data2$AgeAtdiag<66)
  age3 <- subset(data2, data2$year==(2000+i) & data2$AgeAtdiag >= 66 & data2$AgeAtdiag < 72)
  age4 <- subset(data2, data2$year==(2000+i) & data2$AgeAtdiag >= 72)
}

#unfinished#
  
  
############################################################
#group patients based on the centre where they did surgery. If the centre was a low volume one, flag patient as 1.
#low-med 2; med-high 3; high 4; vhigh 5.

  
temp1<- 0


for (i in 1:14){
    
  sub_pan <- subset(data3, data3$surg_date==(2000+i))
  sub_year <- subset(centre_volumn, centre_volumn$year==(2000+i))
    
  for (j in 1:nrow(sub_pan)){
      
    temp <- match(sub_pan$Centre[j], sub_year$centre)
    sub_pan$flag[j] <- sub_year$volumn[temp] 
      
    if (sub_pan$flag[j]<=3) data3$flag[j+temp1] <- 1
    if (sub_pan$flag[j]>3 & sub_pan$flag[j]<=11) data3$flag[j+temp1] <- 2
    if (sub_pan$flag[j]>11 & sub_pan$flag[j]<=30) data3$flag[j+temp1] <- 3
    if (sub_pan$flag[j]>30 & sub_pan$flag[j]<=49) data3$flag[j+temp1] <- 4
    if (sub_pan$flag[j]>=50) data3$flag[j+temp1] <- 5
  }
    
    temp1 <- temp1+nrow(sub_pan)
}
  
#plot OS
  km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 1.5, cex.lab = 1.5, cex.axis = 1.5)
  legend("topright", c("Low volume centre", "Low-med volume centre", "Med-high volume centre", "High volume centre", "Vhigh volume centre"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Long-term survival on centre volume", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  
#plot 90day mortality  
  km <- survfit(Surv(data3$Survdiag, data3$`X90_day_death` == 1) ~ data3$flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$`X90_day_death` == 1) ~ data3$flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 2.5, font = 2, cex.lab = 1.5, cex.axis = 1.3, xmax = 90, ymin = 0.8)
  legend("bottomleft", c("Low volume centre", "Low-med volume centre", "Med-high volume centre", "High volume centre", "Vhigh volume centre"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=1.8,cex=1.1)
  titleString = paste("90-day Survival probability on centre volume", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)

############################################################
  
  #assign centre volume to each patient
  temp1<- 0
  data3$centre_volume <- 0
  
  for (i in 1:14){
    
    sub_pan <- subset(data3, data3$surg_date==(2000+i))
    sub_year <- subset(centre_volumn, centre_volumn$year==(2000+i))
    
    for (j in 1:nrow(sub_pan)){
      
      temp <- match(sub_pan$Centre[j], sub_year$centre)
      data3$centre_volume[j+temp1] <- sub_year$volumn[temp] 
      
      
    }
    
    temp1 <- temp1+nrow(sub_pan)
  }

########################################################################
  
  
  
  

############################################################
#Analysis on Charlson score
data3$charlson_flag <- 0
  
for (i in 1:nrow(data3)){
  if (data3$charlson_score[i] >=0 & data3$charlson_score[i] < 5) data3$charlson_flag[i] <- 1
  if (data3$charlson_score[i] >=5 & data3$charlson_score[i] < 10) data3$charlson_flag[i] <- 2
  if (data3$charlson_score[i] >=10 & data3$charlson_score[i] < 20) data3$charlson_flag[i] <- 3
  if (data3$charlson_score[i] >=20) data3$charlson_flag[i] <- 4
}  

#KM curve plotting
  #90day
  km <- survfit(Surv(data3$Survdiag, data3$`90_day_death` == 1) ~ data3$charlson_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$`90_day_death` == 1) ~ data3$charlson_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 3, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2, xmax = 90, ymin = 0.70)
  legend("bottomleft", c("Charlson score [0,5)", "Charlson score [5,10)", "Charlson score [10,20)", "Charlson score [20,max]"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("90-day Survival probability on Charlson score", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  
  #OS
  km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$charlson_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$charlson_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 3, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2)#, xmax = 90, ymin = 0.9)
  legend("topright", c("Charlson score [0,5)", "Charlson score [5,10)", "Charlson score [10,20)", "Charlson score [20,max]"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Survival probability on Charlson score", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  
##########################################################
  
  
  
###########################################################
#Analysis on imd quintile
  #KM curve plotting
  #90day
  km <- survfit(Surv(data3$Survdiag, data3$`90_day_death`) ~ data3$imd_quintile, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$`90_day_death`) ~ data3$imd_quintile, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2, xmax = 90, ymin = 0.87)
  legend("bottomleft", c("imd quintile = 1", "imd quintile = 2", "imd quintile = 3", "imd quintile = 4","imd quintile = 5"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("90-day Survival probability on imd", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  
  #OS
  km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$imd_quintile, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$imd_quintile, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2)
  legend("topright", c("imd quintile = 1", "imd quintile = 2", "imd quintile = 3", "imd quintile = 4","imd quintile = 5"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Survival probability on imd", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)
  
  ###########################################################  

###########################################################  
#Analysis on ethnicity
  
#group ethnicity
data3$eth_flag <- 0

for (i in 1:nrow(data3)){
  if (data3$ethnos[i] =='A'| data3$ethnos[i] =='C' | data3$ethnos[i] =='B' | data3$ethnos[i] == 0) data3$eth_flag[i]<-1 # White
  if (data3$ethnos[i] =='H'| data3$ethnos[i] =='J' | data3$ethnos[i] =='K' | data3$ethnos[i] =='L' |data3$ethnos[i] == 5) data3$eth_flag[i]<-2 # Asian
  if (data3$ethnos[i] =='M'| data3$ethnos[i] =='N' | data3$ethnos[i] =='P' | data3$ethnos[i] ==1 |data3$ethnos[i] == 3) data3$eth_flag[i]<-3 # Black
  if (data3$ethnos[i] =='R'| data3$ethnos[i] =='S' |data3$ethnos[i] == 8) data3$eth_flag[i] <- 4 # Chinese & other
  if (data3$ethnos[i]=='D'|data3$ethnos[i]=='E'|data3$ethnos[i] =='F'|data3$ethnos[i]=='G'|data3$ethnos[i] =='X'|data3$ethnos[i] =='Z'|data3$ethnos[i]==9) data3$eth_flag[i]<-5 #mixed & unknown
}
  
  #KM curve plotting
  #90day
  km <- survfit(Surv(data3$Survdiag, data3$`90_day_death`) ~ data3$eth_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$`90_day_death`) ~ data3$eth_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2, xmax = 90, ymin = 0.85)
  legend("bottomleft", c("White", "Asian", "Black", "Chinese & other","Mixed & unknown"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("90-day Survival probability on ethnicity", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)  
  
  
  #OS
  km <- survfit(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$eth_flag, data = data3)
  
  survivalDiff <- survdiff(Surv(data3$Survdiag, data3$deathflag == 1) ~ data3$eth_flag, data = data3, rho = 0) # rho = 0 gives no weight to any of the values
  chiSquared   <- pchisq(survivalDiff$chisq, 4, lower.tail=FALSE) # second entry is number of clusters-1
  
  par(mar=c(5,5,2,3))
  plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
       lwd = 3, font = 2, cex.lab = 1.9, cex.axis = 2)
  legend("topright", c("White", "Asian", "Black", "Chinese & other","Mixed & unknown"), lty = 1,lwd= 4,
         col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
  titleString = paste("Survival probability of pancreatic cancer on ethnicity", 
                      " (pValue=", format(chiSquared, scientific=TRUE, digits=3), ")",sep="")
  title(titleString)  
  
#split features  
data3$eth_white <- 0
data3$eth_mixed <- 0
data3$eth_black <- 0
data3$eth_asian <- 0
data3$eth_other <- 0  
for (i in 1:nrow(data3)){
  if (data3$ethnos[i] =='A'| data3$ethnos[i] =='C' | data3$ethnos[i] =='B' | data3$ethnos[i] == 0) 
        data3$eth_white[i]=1 & data3$eth_mixed[i]=0 & data3$eth_black[i]=0 & data3$eth_asian[i]=0 & data3$eth_other[i]=0  # White
  if (data3$ethnos[i] =='H'| data3$ethnos[i] =='J' | data3$ethnos[i] =='K' | data3$ethnos[i] =='L' |data3$ethnos[i] == 5)
        data3$eth_white[i]=0 & data3$eth_mixed[i]=0 & data3$eth_black[i]=0 & data3$eth_asian[i]=1 & data3$eth_other[i]=0 # Asian
  if (data3$ethnos[i] =='M'| data3$ethnos[i] =='N' | data3$ethnos[i] =='P' | data3$ethnos[i] ==1 |data3$ethnos[i] == 3)
        data3$eth_white[i]=0 & data3$eth_mixed[i]=0 & data3$eth_black[i]=1 & data3$eth_asian[i]=0 & data3$eth_other[i]=0# Black
  if (data3$ethnos[i] =='R'| data3$ethnos[i] =='S' |data3$ethnos[i] == 8)
        data3$eth_white[i]=0 & data3$eth_mixed[i]=0 & data3$eth_black[i]=0 & data3$eth_asian[i]=0 & data3$eth_other[i]=1 # Chinese & other
  if (data3$ethnos[i]=='D'|data3$ethnos[i]=='E'|data3$ethnos[i] =='F'|data3$ethnos[i]=='G'|data3$ethnos[i] =='X'|data3$ethnos[i] =='Z'|data3$ethnos[i]==9)
        data3$eth_white[i]=0 & data3$eth_mixed[i]=1 & data3$eth_black[i]=0 & data3$eth_asian[i]=0 & data3$eth_other[i]=0 #mixed & unknown
}#why doesn't work???
  
for (i in 1:nrow(data3)){
  if (data3$ethnos[i] =='A'| data3$ethnos[i] =='C' | data3$ethnos[i] =='B' | data3$ethnos[i] == 0) 
    data3$eth_white[i]<- 1  # White
    if (data3$ethnos[i] =='H'| data3$ethnos[i] =='J' | data3$ethnos[i] =='K' | data3$ethnos[i] =='L' |data3$ethnos[i] == 5)
      data3$eth_asian[i] <- 1 # Asian
      if (data3$ethnos[i] =='M'| data3$ethnos[i] =='N' | data3$ethnos[i] =='P' | data3$ethnos[i] ==1 |data3$ethnos[i] == 3)
        data3$eth_black[i] <- 1# Black
        if (data3$ethnos[i] =='R'| data3$ethnos[i] =='S' |data3$ethnos[i] == 8)
          data3$eth_other[i] <- 1 # Chinese & other
          if (data3$ethnos[i]=='D'|data3$ethnos[i]=='E'|data3$ethnos[i] =='F'|data3$ethnos[i]=='G'|data3$ethnos[i] =='X'|data3$ethnos[i] =='Z'|data3$ethnos[i]==9)
             data3$eth_mixed[i] <- 1#mixed & unknown
}
###########################################################   
  
  
###########################################################   
# Integrated analysis on patients based on the centre volume (looking at patient median age/imd/Charlson on each volume group)
cen_low <- subset(data3, data3$flag==1)
cen_lowmed <- subset(data3, data3$flag==2)
cen_medhigh <- subset(data3, data3$flag==3)
cen_lowmed <- subset(data3, data3$flag==4)
cen_lowmed <- subset(data3, data3$flag==5)
###########################################################    
  
########################################################### 
#survival after 90 days - cancer related survival (remove all the patients died within 90 days)
table1 <- subset(data3, data3$Survdiag > 90)
table1$Survdiag <- table1$Survdiag-90

km <- survfit(Surv(table1$Survdiag, table1$deathflag == 1) ~ flag, data = table1)

par(mar=c(5,5,2,3))
plot(km, xlab = "Days", ylab = "Survival Probability", col = c("red", "blue", "chocolate", "lightpink","darkgreen"),
     lwd = 2, font = 2, cex.lab = 1.9, cex.axis = 2, xmax = 1825, ymin = 0)
legend("bottomleft", c("Low volumn centre", "Low-med volumn centre", "Med-high volumn centre", "High volumn centre", "Vhigh volumn centre"), lty = 1,lwd= 4,
       col = c("red", "blue", "chocolate", "lightpink", "darkgreen"), bty = "n", text.font=2,cex=1.5)
titleString = paste("5 year cancer related survival probability of pancreatic cancer on centre volume")
title(titleString)  
##########################################################



###########################################################
#fit cox model
# call the Cox function see the effect of covariate age on overal survival
library(survival)
  cox_fit <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ factor(flag), data = data3, method="breslow") 
summary(cox_fit)

# test the assumptions of proportional hazard and linearity
ph_assump <-cox.zph(cox_fit)#, transform ='log', global=TRUE)
ph_assump
plot(ph_assump[1,])
abline(h=0, lty=3)

# check linearity
res <- residuals(cox_fit, type='martingale')
X <- as.matrix(data3[,("charlson_score")]) # matrix of covariates
plot(X[,1], res, xlab=c("Charlson score")[1], ylab="residuals")
abline(h=0, lty=2) + lines(lowess(X[,1], res, iter=0))

model_cox <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ sex+AgeAtdiag+eth_other
               +eth_asian+eth_black+eth_white+charlson_score+factor(flag)+imd_quintile, data = data3, method = "breslow") 

#model_cox <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ sex+AgeAtdiag+factor(eth_flag)+charlson_score+factor(flag)+factor(imd_quintile), data = data3, method = "breslow") 
summary(model_cox)
cox.zph(model_cox)
plot(cox.zph(model_cox))
abline(h=0, lty=3)

library(MASS)
stepAIC(model_cox, direction=c("backward")) #AIC
stepAIC(model_cox, k=log(161)) #BIC
anova(model_cox) #Deviance

model_AIC <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ AgeAtdiag+eth_asian+eth_white+charlson_score+centre_volume+imd_quintile, data = data3, method = "breslow")
model_AIC <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ AgeAtdiag+eth_asian+eth_white+charlson_score+flag+imd_quintile, data = data3, method = "breslow")
model_BIC <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ AgeAtdiag+eth_asian+charlson_score+centre_volume+imd_quintile, data = data3, method = "breslow")


summary(model_AIC)

survConcordance(Surv(data3$Survdiag, data3$deathflag)~predict(model_AIC), data= data3)
survConcordance(Surv(data3$Survdiag, data3$deathflag)~predict(model_BIC), data= data3)

#######################################################################################




#########fit glmnet model##############
library(glmnet)
library(survival)

data_feature <- cbind(data3$surg_date, data3$sex, data3$AgeAtdiag, data3$charlson_score, data3$imd_quintile, data3$flag, data3$eth_white, data3$eth_mixed, data3$eth_black, data3$eth_asian, data3$eth_other, data3$deathflag, data3$Survdiag)
colnames(data_feature) <- c('surg_date','sex','age','charlson','imd_quintile','centre','eth_white','eth_mixed','eth_black','eth_asian','eth_other','deathflag','Survdiag')
data_feature <- data.frame(data_feature)

#set1 <- subset(data_feature, data_feature$surg_date<=2005)
set1.1 <- cbind(Nset1$sex, Nset1$age,Nset1$imd_quintile, Nset1$charlson, Nset1$centre, Nset1$eth_white,Nset1$eth_mixed, Nset1$eth_black, Nset1$eth_asian,Nset1$eth_other)
set1.1 <- data.frame(set1.1)
colnames(set1.1) <- c('sex','age','imd_quintile','charlson','centre','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')

#set2 <- subset(data_feature, data_feature$surg_date>2005 & data_feature$surg_date<=2010)
set2.2 <- cbind(Nset2$sex, Nset2$age,Nset2$imd_quintile, Nset2$charlson, Nset2$centre, Nset2$eth_white,Nset2$eth_mixed, Nset2$eth_black, Nset2$eth_asian,Nset2$eth_other)
colnames(set2.2) <- c('sex','age','imd_quintile','charlson','centre','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')

#set3 <- subset(data_feature, data_feature$surg_date>2010)
set3.3 <- cbind(Nset3$sex, Nset3$age,Nset3$imd_quintile, Nset3$charlson, Nset3$centre, Nset3$eth_white,Nset3$eth_mixed, Nset3$eth_black, Nset3$eth_asian,Nset3$eth_other)
colnames(set3.3) <- c('sex','age','imd_quintile','charlson','centre','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')

trainingData <- rbind(Nset3,Nset2)[,-1]

for(i in 1:nrow(trainingData)){
if (trainingData$Survdiag[i]==0) trainingData$Survdiag[i]=0.0001
}
testData <- set1.1

trainingData2 <- rbind(Nset1,Nset3)[,-1]

for(i in 1:nrow(trainingData)){
  if (trainingData2$Survdiag[i]==0) trainingData2$Survdiag[i]=0.0001
}
testData2 <- set2.2

trainingData3 <- rbind(Nset1,Nset2)[,-1]

for(i in 1:nrow(trainingData)){
  if (trainingData3$Survdiag[i]==0) trainingData3$Survdiag[i]=0.0001
}
testData3 <- set3.3



##TRAIN MODEL
model_glm <- glmnet(as.matrix(rbind(set2.2,set1.1)), Surv(trainingData$Survdiag, trainingData$deathflag), family="cox", maxit=1e4, alpha=1)
plot.glmnet(model_glm)
#plot(model_glm$lambda)
#model_glm  #call the produce
#summary(model_glm)
#model_glm$beta
#coef(model_glm)[,47]  #looking at the coeff. for the 47th lambda



cv.fit <- cv.glmnet(as.matrix(rbind(set1.1,set2.2)),Surv(trainingData$Survdiag, trainingData$deathflag), family="cox", maxit=1e4, alpha=1)
plot.cv.glmnet(cv.fit)
best_lambda <- cv.fit$lambda.min


##MAKE PREDICTIONS
predictions <- predict(model_glm, as.matrix(testData), type="response", s=best_lambda)
predictions2 <- predict(model_glm, as.matrix(testData), type="response", s=best_lambda)
predictions3 <- predict(model_glm, as.matrix(testData), type="response", s=best_lambda)
plot(predictions)
predictions
data3$glm90day<-0
data3$glmCV<-0

for (i in 0:2666){
  data3$glmCV[3*i+1]<-predictions[i+1]
  data3$glmCV[3*i+2]<-predictions2[i+1]
  data3$glmCV[3*i+3]<-predictions3[i+1]
}
data3$glmCV[8002]<-predictions[2668]

survConcordance(Surv(Survdiag, deathflag) ~ glmCV, data3)

####non CV GLM####
glmdata<- cbind( data3$sex, data3$AgeAtdiag, data3$charlson_score, data3$imd_quintile, data3$flag, data3$eth_white, data3$eth_mixed, data3$eth_black, data3$eth_asian, data3$eth_other)
colnames(glmdata) <- c('sex','age','imd_quintile','charlson','centre','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')
glmdata<-as.matrix(glmdata)

for(i in 1:nrow(glmdata)){
  if (data3$Survdiag[i]==0) data3$Survdiag[i]=0.0001
}

model_glm <- glmnet(glmdata, Surv(data3$Survdiag, data3$deathflag), family="cox", maxit=1e4, alpha=1)
cv.fit <- cv.glmnet(glmdata,Surv(data3$Survdiag, data3$deathflag), family="cox", maxit=1e4, alpha=1)
best_lambda <- cv.fit$lambda.min
prediction<- predict(model_glm, glmdata, type="response", s=best_lambda)

data3$predict_glm<-prediction
survConcordance(Surv(Survdiag, deathflag) ~ predict_glm, data3)


####ends###


#calculate c-index

model_glm <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ AgeAtdiag+charlson_score+flag+eth_white+eth_asian+eth_other+imd_quintile, data = data3, method = "breslow")

survConcordance(Surv(Survdiag, deathflag) ~ predict_glm1, data3)




#  get predictions by using coefficients 
beta  <- as.vector( t(coef(cv.fit,s=best_lambda))) #equivalent to predict(..type='coef')

# Coefficients are returned on the scale of the original data. 
# note we need to add column  of 1s for intercept
testX <- cbind(1,testData) 
glm_predict  <- data.frame(testX)*beta 

# check by plotting predictions  
plot(glm_predict,predictions)


#------------------------------------------------------------------------#
########## randomSurvivalForest #############
#library(randomForestSRC)
library(survival)
#library(randomForest)
library(party)
#library(ipred)
memory.limit(3048)
#options(rf.cores = -1) #assess max number of cores on the machine for computation
#options(rf.cores=detectCores()-1, mc.cores=detectCores()-1)


data_feature <- cbind(data3$surg_date, data3$sex, data3$AgeAtdiag, data3$charlson_score, data3$imd_quintile, data3$flag, data3$eth_white, data3$eth_mixed, data3$eth_black, data3$eth_asian, data3$eth_other, data3$deathflag, data3$Survdiag)
colnames(data_feature) <- c('surg_date','sex','age','charlson','imd_quintile','centre','eth_white','eth_mixed','eth_black','eth_asian','eth_other','deathflag','Survdiag')
data_feature <- data.frame(data_feature)


set1.1 <- cbind(Nset1$sex, Nset1$age,Nset1$imd_quintile, Nset1$charlson, Nset1$centre, Nset1$eth_white,Nset1$eth_mixed, Nset1$eth_black, Nset1$eth_asian,Nset1$eth_other)
set1.1 <- data.frame(set1.1)
colnames(set1.1) <- c('sex','age','imd_quintile','charlson','CentreVol_group','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')
#colnames(Nset3) <- c('sex','age','imd_quintile','charlson','CentreVol_group','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')


set2.2 <- cbind(Nset2$sex, Nset2$age,Nset2$imd_quintile, Nset2$charlson, Nset2$centre, Nset2$eth_white,Nset2$eth_mixed, Nset2$eth_black, Nset2$eth_asian,Nset2$eth_other)
colnames(set2.2) <- c('sex','age','imd_quintile','charlson','CentreVol_group','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')


set3.3 <- cbind(Nset3$sex, Nset3$age,Nset3$imd_quintile, Nset3$charlson, Nset3$centre, Nset3$eth_white,Nset3$eth_mixed, Nset3$eth_black, Nset3$eth_asian,Nset3$eth_other)
colnames(set3.3) <- c('sex','age','imd_quintile','charlson','CentreVol_group','eth_white', 'eth_mixed','eth_black','eth_asian','eth_other')

trainingData <- rbind(Nset2,Nset3)
names(trainingData)[6]<-c('CentreVol_group')

for(i in 1:nrow(trainingData)){
  if (trainingData$Survdiag[i]==0) trainingData$Survdiag[i]=0.0001
}
testData <- data.frame(set1.1)


trainingData2 <- rbind(Nset3,Nset1)
names(trainingData2)[6]<-c('CentreVol_group')
for(i in 1:nrow(trainingData2)){
  if (trainingData2$Survdiag[i]==0) trainingData2$Survdiag[i]=0.0001
}
testData2 <- data.frame(set2.2)



trainingData3 <- rbind(Nset2,Nset1)
names(trainingData3)[6]<-c('CentreVol_group')

for(i in 1:nrow(trainingData3)){
  if (trainingData3$Survdiag[i]==0) trainingData3$Survdiag[i]=0.0001
}
testData3 <- data.frame(set3.3)



model_cforest <- cforest(Surv(Survdiag, deathflag) ~ sex+age+imd_quintile+charlson+CentreVol_group+eth_white+eth_mixed+
                           eth_black+eth_asian+eth_other, data = trainingData3, control = cforest_unbiased(ntree = 300))

prediction_cforest<- treeresponse(model_cforest,newdata=testData,OOB=TRUE) #which one to use?
prediction_cforest1<- predict(model_cforest,newdata=testData,type='response', OOB=TRUE)
prediction_cforest2<- predict(model_cforest,newdata=testData2,type='response', OOB=TRUE)
prediction_cforest3<- predict(model_cforest,newdata=testData3,type='response', OOB=TRUE)

data3$forest_CV <-0

for (i in 0:2666){
  data3$forest_CV[3*i+1]<-prediction_cforest[i+1]
  data3$forest_CV[3*i+2]<-prediction_cforest2[i+1]
  data3$forest_CV[3*i+3]<-prediction_cforest3[i+1]
}
data3$forest_CV[8002]<-prediction_cforest[2668]

survConcordance(Surv(Survdiag, deathflag) ~forest_CV, data=data3)

#table(Nset1$Survdiag,prediction_cforest)

survConcordance.fit(Surv(Nset1$Survdiag, Nset1$deathflag),prediction_cforest)




varimp(model_cforest)#takes tooooo loooooooong

varimp(model_cforest,conditional = TRUE)



model_ctree<- ctree(Surv(Survdiag, deathflag) ~ sex+age+imd_quintile+charlson+CentreVol_group+eth_white+eth_mixed+
                       eth_black+eth_asian+eth_other, data = trainingData)
plot(model_ctree)
response_tree <- treeresponse(model_ctree,newdata=testData)


#####the following codes are not successful: not enough RAM. SO DO NOT RUN!

model_rfs <- rfsrc(Surv(Survdiag, deathflag) ~ sex+age+imd_quintile+charlson+centre+eth_white+eth_mixed+
                     eth_black+eth_asian+eth_other, data = trainingData, ntree =40,nsplit=10, ntime=100)



plot(model_rfs)
plot.survival(model_rfs)

prediction_rfs <- predict.rfsrc(model_rfs, data.frame(testData))
print(prediction_rfs)

var.select(model_rfs)
predict_rfs<-0
predict_glm[1:2189]<-predictions
predict_glm[2190:5419]<-predictions
predict_glm[5420:8002]<-predictions
#DO NOT RUN END



###concordance index
library("Hmisc")

rcorr.cens(x=predict(model_rfs), S=Surv(data3$Survdiag, data3$deathflag)) #returns error

rcorrcens(Surv(Survdiag, deathflag) ~ AgeAtdiag+imd_quintile+charlson_score+factor(flag), data = data3)

model_rfs <- coxph(Surv(data3$Survdiag, data3$deathflag) ~ AgeAtdiag+charlson_score+flag+imd_quintile, data = data3, method = "breslow")

survConcordance(Surv(data3$Survdiag, data3$deathflag)~predict(model_rfs), data= data3)












#######Keith request postoperative death analysis#############
table2 <- subset(data3, data3$Survdiag<=90)
table3 <- subset(table2, table2$Survdiag<=30)
table4 <- subset(table2, table2$Survdiag>30)
#################################################################





############Two-sample test using Gaussian Process##########
library(DEtime)



### calculating the loglikelihood ratio for these tested genes. the result is saved into DEtime_rank.txt
res_rank <- DEtime_rank(times = UK_MotalityRate$X, ControlData = UK_MotalityRate$High, PerturbedData=UK_MotalityRate$Med.High,
            savefile=TRUE)#not sure if i need this

### get the index of these data with loglikelihood ratio larger than 4
idx <- which(res_rank[,2]>4)

### go on with the perturbation time inference if some of the data has passed the threshould test 
if (length(idx)>0){
  res <- DEtime_infer(times = UK_MotalityRate$X, ControlData = UK_MotalityRate$High, PerturbedData=UK_MotalityRate$Med.High,
                      replicate_no=0, gene_no=14,times_test=UK_MotalityRate$X)
  
  ### Print a summary of the results
  print_DEtime(res)
  ### plot results for all the genes
  plot_DEtime(res)
}
############################################################

#Resplit the patients more evenly for C-V
Nset1<-NULL
Nset2<-NULL
Nset3<-NULL

for(i in 0:2666){
  Nset1<-rbind(Nset1,data_feature[3*i+1,])
  Nset2<-rbind(Nset2,data_feature[3*i+2,])
  Nset3<-rbind(Nset3,data_feature[3*i+3,])
}

Nset1<-rbind(Nset1,data_feature[8002,])

##--------------------------------------------------------------##
##               90day survival predition                       ##
##--------------------------------------------------------------##

###glm###
glm90dayprediction <- subset(data3, data3$Survdiag<=90)
survConcordance(Surv(Survdiag, deathflag) ~ glmCV, glm90dayprediction)


###cox
cox90dayprediction <- subset(data3, data3$Survdiag<=90)
model_AIC <- coxph(Surv(cox90dayprediction$Survdiag, cox90dayprediction$deathflag) ~ AgeAtdiag+eth_asian+eth_white+charlson_score+centre_volume+imd_quintile, data = cox90dayprediction, method = "breslow")
model_AIC <- coxph(Surv(cox90dayprediction$Survdiag, cox90dayprediction$deathflag) ~ AgeAtdiag+eth_asian+eth_white+charlson_score+flag+imd_quintile, data = cox90dayprediction, method = "breslow")
model_BIC <- coxph(Surv(cox90dayprediction$Survdiag, cox90dayprediction$deathflag) ~ AgeAtdiag+eth_asian+charlson_score+centre_volume+imd_quintile, data = cox90dayprediction, method = "breslow")


summary(model_AIC)

survConcordance(Surv(cox90dayprediction$Survdiag, cox90dayprediction$deathflag)~predict(model_AIC), data= cox90dayprediction)
survConcordance(Surv(cox90dayprediction$Survdiag, cox90dayprediction$deathflag)~predict(model_BIC), data= cox90dayprediction)

###random forest


##--------------------------------------------------------------##
##               Univariate analysis                            ##
##--------------------------------------------------------------##
library(survival)
survConcordance(Surv(data3$Survdiag, data3$deathflag)~AgeAtdiag, data= data3)

