####Code for extracting diurnal SD activity profile from minute level data####
#library(ActCR)
#path <- "E:/JHUPD/Deliverables/data/data/minute-csv"
#setwd(path)
#filelist<-list.files("E:/JHUPD/Deliverables/data/data/minute-csv")
#myfiles <-lapply(filelist, read.csv)
#subjdf<-matrix(0,nrow = 92,ncol=5)
#subjdf<-as.data.frame(subjdf)
#actmat<-matrix(0,nrow = 92,ncol=144)
#for(i in 1:92)
#{
#  mydata<-myfiles[[i]]
#  mydata$ADStatus<-ifelse(mydata$ADStatus=="Yes",1,0)
#  mydata$Sex<-ifelse(mydata$Sex=="Male",1,0)
#  #Date, Time , Vector.Magnitude
#  subdata<-mydata[,c("Date","Time","Vector.Magnitude")]
#  library(chron)
#  subdata$Time<-60 * 24 * as.numeric(times(subdata$Time))
#  n<-nlevels(subdata$Date)
#  lev<-levels(subdata$Date)
#  tbtick<-seq(0,1440,by=10)
#  actvec<-c()
#  l=length(tbtick)-1
#  for (j in 1:l)
#  {
#    tempdataj<-subdata[subdata$Time>=tbtick[j]&subdata$Time <tbtick[j+1],]
#    actvec[j]<-sd(tempdataj$Vector.Magnitude)
#  }
# subjdf[i,]<-c(mydata$id[1],mydata$Age[1],mydata$ADStatus[1],mydata$Sex[1],mydata$YearsOfEducation[1])
#  actmat[i,]<-actvec
#  }


#names(subjdf)<-c("id","age","adstatus","Sex","education")
#subjdf[2,]$education<-c(NA)
##################
#subjdf$actmat<-actmat
#save(subjdf,file="actminutelevel_144_sd.Rdata")


###Loading pre extracted data now###
load("actminutelevel_144_sd.RData")
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
tbtick<-seq(0,1440,by=10)
binmid<-tbtick+5
binmid<-binmid[-145]
cogdata<-read.csv("ACCEL cognitive data.csv")###aceel cogdata
ind<-which(cogdata$ï..ID%in%subjdf$id)
cogdatasub<-cogdata[ind,]
cogdatasub$id<-cogdatasub$ï..ID
cogdatasub<-cogdatasub[c(1:4,89:92,5:88),]
subjdf$ATTN<-cogdatasub$ATTN
subjdf$VM<-cogdatasub$VM
subjdf$ExecFunction<-cogdatasub$ExecFunction

library(refund)
###############cognitive scores as outcome###########
fit.lf1 <- pfr(ATTN~ age+Sex+education+lf(actmat,argvals=binmid/60, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf1)
plot(fit.lf1,xlab="t",ylab=expression(paste(beta(t))),main="effect on ATTN")
abline(h=0)
fit.lf2 <- pfr(VM~ age+Sex+education+lf(actmat,argvals=binmid/60, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf2)
plot(fit.lf2,xlab="t",ylab=expression(paste(beta(t))),main="effect on VM")
abline(h=0)
fit.lf3 <- pfr(ExecFunction~ age+Sex+education+lf(actmat,argvals=binmid/60, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf3)
plot(fit.lf3,xlab="t",ylab=expression(paste(beta(t))),main="effect on ExecFunction")
abline(h=0)
