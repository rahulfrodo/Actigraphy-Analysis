###Analysis in Section 9###
###Extracting subject speci
####Load pre-extracted subject-specific quantile functions####
load("actminutelevel_qf.RData")
#summary(subjdf)
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
p<-seq(0,1,l=101)
indAD<-which(subjdf$adstatus==1)
actad<-subjdf$actmat[indAD,]
actcontrol<-subjdf$actmat[-indAD,]
cnavg<-colMeans(actcontrol)
adavg<-colMeans(actad)
l1<-loess.smooth(p, cnavg,span = 0.5)
l2<-loess.smooth(p, adavg,span=0.5)
plot(l1$x,l1$y,type="l",col=4,xlab="time of day",ylab="average activity")
lines(l2$x,l2$y,type="l",col=2,lty=2)  
legend('topleft',c("AD ","control") , 
       lty=c(2,1), col=c("red", "blue"), bty='n', cex=.75)

library(refund)
p<-seq(0,1,l=101)
fit.lf <- pfr(adstatus~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2),data = subjdf,family="binomial")
summary(fit.lf)
plot(fit.lf,xlab="p",ylab=expression(paste(beta(p))),,main="effect on log odds of AD")
abline(h=0)

load("actminutelevel_qf.RData")
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
cogdata<-read.csv("ACCEL cognitive data.csv")###aceel cogdata
ind<-which(cogdata$ï..ID%in%subjdf$id)
cogdatasub<-cogdata[ind,]
cogdatasub$id<-cogdatasub$ï..ID
cogdatasub<-cogdatasub[c(1:4,89:92,5:88),]
subjdf$ATTN<-cogdatasub$ATTN
subjdf$VM<-cogdatasub$VM
subjdf$ExecFunction<-cogdatasub$ExecFunction

library(refund)
p<-seq(0,1,l=101)
###############cognitive scores as outcome###########
fit.lf1 <- pfr(ATTN~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf1) #ADJ RSQ 0.218, PVAL=0.0182
plot(fit.lf1,xlab="p",ylab=expression(paste(beta(p))),main="effect on ATTN")
abline(h=0)
fit.lf2 <- pfr(VM~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf2) #0.375 , pval=0.000229 ***
plot(fit.lf2,xlab="p",ylab=expression(paste(beta(p))),main="effect on VM")
abline(h=0)
fit.lf3 <- pfr(ExecFunction~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2),data = subjdf)
summary(fit.lf3) #adj rsq 0.347, pval=0.00438 **
plot(fit.lf3,xlab="p",ylab=expression(paste(beta(p))),main="effect on ExecFunction")
abline(h=0)

#Extension using both temporal SD profile and subject specific quantile function

load("actminutelevel_qf.RData")
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
qfdf<-subjdf
rm(subjdf)
load("actminutelevel_144_sd.RData")
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
sddf<-subjdf
rm(subjdf)
qfdf$sd<-sddf$actmat
colnames(qfdf$actmat) <- paste(1:101)
colnames(qfdf$sd) <- paste(1:144)
tbtick<-seq(0,1440,by=10)
binmid<-tbtick+5
binmid<-binmid[-145]
cogdata<-read.csv("ACCEL cognitive data.csv")###aceel cogdata
ind<-which(cogdata$ï..ID%in%qfdf$id)
cogdatasub<-cogdata[ind,]
cogdatasub$id<-cogdatasub$ï..ID
cogdatasub<-cogdatasub[c(1:4,89:92,5:88),]
qfdf$ATTN<-cogdatasub$ATTN
qfdf$VM<-cogdatasub$VM
qfdf$ExecFunction<-cogdatasub$ExecFunction

library(refund)
p<-seq(0,1,l=101)
###############cognitive scores as outcome###########
fit.lf1 <- pfr(ATTN~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2)+lf(sd,argvals=binmid/60, k=12, bs="ps",m=2),data = qfdf)
summary(fit.lf1) 
par(mfrow=c(1,2))
plot(fit.lf1,select=1,ylim=c(-0.1,0.1),xlab="p",main="quantile effect",ylab=expression(paste(beta(p))))
abline(h=0)
plot(fit.lf1,select=2,ylim=c(-0.001,0.001),xlab="t",main="temporal effect sd",ylab=expression(paste(beta(t))))
abline(h=0)
#fit.lf2 <- pfr(VM~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2)+lf(sd,argvals=binmid/60, k=12, bs="ps",m=2),data = qfdf)
#summary(fit.lf2) 
#fit.lf3 <- pfr(ExecFunction~ age+Sex+education+lf(actmat,argvals=p, k=12, bs="ps",m=2)+lf(sd,argvals=binmid/60, k=12, bs="ps",m=2),data = qfdf)
#summary(fit.lf3)  


