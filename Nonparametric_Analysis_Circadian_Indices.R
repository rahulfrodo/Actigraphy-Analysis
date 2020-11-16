# library(ActCR)
# x is a activity vector
# raval = RA(x = x, method = "average")
# This extracts the circadian parameters.
####original code for extracting circadian parameters from minute level activity data###

#library(ActCR)
#path <- "E:/JHUPD/Deliverables/data/data/minute-csv"
#setwd(path)
#filelist<-list.files("E:/JHUPD/Deliverables/data/data/minute-csv")
#myfiles <-lapply(filelist, read.csv)
#subjdf<-matrix(NA,nrow = 92,ncol=8)

#for(i in 1:92)
#{
#  mydata<-myfiles[[i]]
#  mydata$ADStatus<-ifelse(mydata$ADStatus=="Yes",1,0)
#  mydata$Sex<-ifelse(mydata$Sex=="Male",1,0)
#  subdata<-mydata[,c("Date","Time","Vector.Magnitude")]
#  library(chron)
#  subdata$Time<-60 * 24 * as.numeric(times(subdata$Time))
#  subdata$Date<-as.factor(subdata$Date)
#  n<-nlevels(subdata$Date)
#  lev<-levels(subdata$Date)
#  datelev<-levels(subdata$Date)
#  tbtick<-seq(0,1440,by=10)
#  l=length(tbtick)-1
#  actmat<-matrix(0,nrow = n,ncol = l)
#  for(k in 1:n){
#    for (j in 1:l)
#    {
#      tempdataj<-subdata[ subdata$Date==datelev[k]&subdata$Time>=tbtick[j]&subdata$Time <tbtick[j+1],] #not having obs on some day
#      actmat[k,j]<-mean(tempdataj$Vector.Magnitude,na.rm =TRUE)
#    }
#  }
#  actmat[is.nan(actmat)] = NA
#  actmeanvec<-colMeans(actmat,na.rm = TRUE)
#  ##replacing NA with mean
#  for(s in 1:ncol(actmat)){
#    actmat[is.na(actmat[,s]), s] <- mean(actmat[,s], na.rm = TRUE)
#  }
#  ISval = IS(x = actmat) 
#  ivval = IV(x = actmeanvec)
#  tbticklong<-seq(0,1439,by=1)
#  llong=length(tbticklong)
#  actlong<-c()
#  for (l in 1:llong)
#  {
#    tempdataj<-subdata[subdata$Time==tbticklong[l],] #not having obs on some day
#    actlong[l]<-mean(tempdataj$Vector.Magnitude,na.rm = TRUE)
#  }
#  actlong[is.nan(actlong)] = NA
#  actlong[is.na(actlong)] = mean(actlong,na.rm = TRUE)
#  raval = RA(x = actlong, method = "average")
#  vec<-c(mydata$id[1],ISval,ivval,raval,mydata$Age[1],mydata$ADStatus[1],mydata$Sex[1],mydata$YearsOfEducation[1])
#  subjdf[i,]<-vec
#  }
#subjdf<-as.data.frame(subjdf)
#names(subjdf)<-c("id","is","iv","ra","age","adstatus","Sex","education")
#summary(subjdf)
#subjdf[2,]$education<-c(NA)
##################
#cogdata<-read.csv(file.choose("ACCEL cognitive data.csv"))###data containing cognitive scores info
#ind<-which(cogdata$ï..ID%in%subjdf$id)
#cogdatasub<-cogdata[ind,]
#cogdatasub$id<-cogdatasub$ï..ID
#dfcomb<-merge(subjdf, cogdatasub, by="id")
#save(dfcomb,file="cogdfcirca.Rdata")

#Loading pre-extracted data with circadian parameters

path <- "E:/JHUPD/Deliverables/data/data"
setwd(path)
load("cogdfcirca.RData")
par(mfrow=c(2,2))
boxplot(is~adstatus,data=dfcomb,names=c("control","ad"),ylab="IS")
boxplot(iv~adstatus,data=dfcomb,names=c("control","ad"),ylab="IV")
boxplot(ra~adstatus,data=dfcomb,names=c("control","ad"),ylab="RA")

par(mfrow=c(2,2))
boxplot(is~Sex,data=dfcomb,names=c("Female","Male"),ylab="IS")
boxplot(iv~Sex,data=dfcomb,names=c("Female","Male"),ylab="IV")
boxplot(ra~Sex,data=dfcomb,names=c("Female","Male"),ylab="RA")

subsetdfcomb<-dfcomb[,c(2,3,4,5,6,7,8,28,32,37)]
plot(subsetdfcomb[,-c(5,6)])


dfcomb$education[2]<-mean(dfcomb$education,na.rm=TRUE)
glm1<-glm(adstatus~age+sex+education,data=dfcomb,family = "binomial")
glm11<-glm(adstatus~age+sex+education+is,data=dfcomb,family = "binomial")
glm12<-glm(adstatus~age+sex+education+iv,data=dfcomb,family = "binomial")
glm13<-glm(adstatus~age+sex+education+ra,data=dfcomb,family = "binomial")
tab_model(glm1,glm11,glm12,glm13,title = "Modelling cognitive status using circadian rhythmicity parameters")


lm11<-lm(ATTN~age+sex+education,data=dfcomb)
lm12<-lm(ATTN~age+sex+education+is,data=dfcomb)
lm13<-lm(ATTN~age+sex+education+iv,data=dfcomb)
lm14<-lm(ATTN~age+sex+education+ra,data=dfcomb)
tab_model(lm11,lm12,lm13,lm14,title = "Modelling cognitive scores of attention using circadian parameters")

lm21<-lm(VM~age+sex+education,data=dfcomb)
lm22<-lm(VM~age+sex+education+is,data=dfcomb)
lm23<-lm(VM~age+sex+education+iv,data=dfcomb)
lm24<-lm(VM~age+sex+education+ra,data=dfcomb)
tab_model(lm21,lm22,lm23,lm24,title = "Modelling cognitive scores of verbal memory using circadian parameters")

lm31<-lm(ExecFunction~age+sex+education,data=dfcomb)
lm32<-lm(ExecFunction~age+sex+education+is,data=dfcomb)
lm33<-lm(ExecFunction~age+sex+education+iv,data=dfcomb)
lm34<-lm(ExecFunction~age+sex+education+ra,data=dfcomb)
tab_model(lm31,lm32,lm33,lm34,title = "Modelling cognitive scores of ececutive function using circadian parameters")