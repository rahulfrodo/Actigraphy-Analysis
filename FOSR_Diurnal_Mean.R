####Code for extracting diurnal mean activity profile from minute level data####
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
#  mydata$Vector.Magnitude<-log(1+mydata$Vector.Magnitude)
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
#    actvec[j]<-mean(tempdataj$Vector.Magnitude)
#  }
# subjdf[i,]<-c(mydata$id[1],mydata$Age[1],mydata$ADStatus[1],mydata$Sex[1],mydata$YearsOfEducation[1])
#  actmat[i,]<-actvec
#  }


#names(subjdf)<-c("id","age","adstatus","Sex","education")
#subjdf[2,]$education<-c(NA)
##################
#subjdf$actmat<-actmat
#save(subjdf,file="actminutelevel_144.Rdata")

###Loading pre extracted data now###
load("actminutelevel_144.RData")
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
tbtick<-seq(0,1440,by=10)
binmid<-tbtick+5
binmid<-binmid[-145]
indAD<-which(subjdf$adstatus==1)
actad<-subjdf$actmat[indAD,]
actcontrol<-subjdf$actmat[-indAD,]
cnavg<-colMeans(actcontrol)
adavg<-colMeans(actad)
l1<-loess.smooth(binmid, cnavg,span = 0.25)
l2<-loess.smooth(binmid, adavg,span = 0.25)
plot(l1$x/60,l1$y,type="l",col="blue",xaxt="n",xlab="time of day",ylab="average activity")
xtick<-seq(0, 24, by=2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 0, pos = 1, xpd = TRUE)
lines(l2$x/60,l2$y,type="l",col="red",lty=2)  
legend('topleft',c("AD ","control") , 
       lty=c(2,1), col=c("red", "blue"), bty='n', cex=.75)

library(refund)
fit <- pffr(actmat~ age+Sex+adstatus+education,yind=(binmid/60), data = subjdf,bs.yindex = list(bs = "ps",
                                                                                                k = 12, m = c(2, 2)), bs.int = list(bs = "ps", k = 12, m = c(2, 2)))
summary(fit)

par(mfrow=c(2,2))
plot(fit,select = 4,ylim=c(-250,100),xaxt="n",ylab = "ad diurnal effect",xlab="time of day")
xtick<-seq(0, 24, by=2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 0, pos = 1, xpd = TRUE)
abline(h=0)
plot(fit,select = 3,xaxt="n",ylim=c(-60,150),ylab = "sex (Male) diurnal effect",xlab="time of day")
xtick<-seq(0, 24, by=2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 0, pos = 1, xpd = TRUE)
abline(h=0)
plot(fit,select = 2,xaxt="n",ylim=c(-15,10),ylab = "age diurnal effect",xlab="time of day")
xtick<-seq(0, 24, by=2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 0, pos = 1, xpd = TRUE)
abline(h=0)
plot(fit,select = 5,xaxt="n",ylim=c(-10,10),ylab = "education diurnal effect",xlab="time of day")
xtick<-seq(0, 24, by=2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 0, pos = 1, xpd = TRUE)
abline(h=0)

fitmat<-predict(fit)  
predtrain<-predict(fit,newdata = subjdf)  #total match with fitmat
newtedataMY<-data.frame("age"=mean(subjdf$age),"Sex"=1,"adstatus"=1,"education"=mean(subjdf$education))
predtMY<-predict(fit,newdata = newtedataMY)
newtedataMN<-data.frame("age"=mean(subjdf$age),"Sex"=1,"adstatus"=0,"education"=mean(subjdf$education))
predtMN<-predict(fit,newdata = newtedataMN)
newtedataFY<-data.frame("age"=mean(subjdf$age),"Sex"=0,"adstatus"=1,"education"=mean(subjdf$education))
predtFY<-predict(fit,newdata = newtedataFY)
newtedataFN<-data.frame("age"=mean(subjdf$age),"Sex"=0,"adstatus"=0,"education"=mean(subjdf$education))
predtFN<-predict(fit,newdata = newtedataFN)
par(mfrow=c(1,2))
tnew<-(binmid/60)
plot(tnew,predtMY,type="o",col="red",xlab="hour",ylab="fitted meandiurnal curve",main="diurnal fit male",ylim = c(0,700))
lines(tnew,predtMN,type="o",col="blue")
legend('topleft',c("ad=Yes","ad=No") , 
       lty=c(1,1), col=c('red', 'blue'),pch=c(1,1), bty='n', cex=.75)
plot(tnew,predtFY,type="o",col="red",xlab="hour",ylab="fitted mean diurnal curve",main="diurnal fit female",ylim=c(0,700))
lines(tnew,predtFN,type="o",col="blue")
legend('topleft',c("ad=Yes","ad=No") , 
       lty=c(1,1), col=c('red', 'blue'),pch=c(1,1), bty='n', cex=.75)