#### Codeblock for MFPCA shown below ######
#library(ActCR)
#path <- "E:/JHUPD/Deliverables/data/data/minute-csv"
#setwd(path)
#filelist<-list.files("E:/JHUPD/Deliverables/data/data/minute-csv")
#myfiles <-lapply(filelist, read.csv)
#subjdf<-matrix(0,nrow = 92,ncol=5)
#subjdf<-as.data.frame(subjdf)
#actmatlist<-list()

#for(i in 1:92)
#{
#  mydata<-myfiles[[i]]
#  mydata$ADStatus<-ifelse(mydata$ADStatus=="Yes",1,0)
#  mydata$Sex<-ifelse(mydata$Sex=="Male",1,0)
 
#  subdata<-mydata[,c("Date","Time","Vector.Magnitude")]
#  library(chron)
#  subdata$Time<-60 * 24 * as.numeric(times(subdata$Time))
#  n<-nlevels(subdata$Date)
#  actmatlist[[i]]<- matrix(0,nrow = n,ncol=144)
#  datelev<-levels(subdata$Date)
#  tbtick<-seq(0,1440,by=10)
#  actvec<-c()
#  l=length(tbtick)-1
#  for(k in 1:n){
#    for (j in 1:l)
#    {
#      tempdataj<-subdata[ subdata$Date==datelev[k]&subdata$Time>=tbtick[j]&subdata$Time <tbtick[j+1],] 
#      actmatlist[[i]][k,j]<-mean(tempdataj$Vector.Magnitude,na.rm =TRUE)
#    }
#  }
#  actmatlist[[i]][is.nan(actmatlist[[i]])]=NA
  ##replacing NA with mean
#  for(s in 1:ncol(actmatlist[[i]])){
#    actmatlist[[i]][is.na(actmatlist[[i]][,s]), s] <- mean(actmatlist[[i]][,s], na.rm = TRUE)
#  }
  
#  subjdf[i,]<-c(mydata$id[1],mydata$Age[1],mydata$ADStatus[1],mydata$Sex[1],mydata$YearsOfEducation[1])
#}
#names(subjdf)<-c("id","age","adstatus","Sex","education")
#summary(subjdf)
#subjdf[2,]$education<-c(NA)
#idvec<-c()
#for (i in 1:92)
#{idvec<-c(idvec,rep(subjdf$id[i],nrow(actmatlist[[i]])))}

#Y<-Reduce(rbind,actmatlist) #932*144
tbtick<-seq(0,1440,by=10)
binmid<-tbtick+5
binmid<-binmid[-145]
#library(refund)
#mfpcafit =  mfpca.sc(Y=Y, id = idvec, twoway = TRUE) 

load("mfpcafit.RData")
lev1score<-mfpcafit$scores$level1
lev2score<-mfpcafit$scores$level2
dim(lev1score)
dim(lev2score)

par(mfrow=c(3,3))
par(oma=c(3,3,3,3),mar=c(3,4,1,1))
#level1ef
plot(binmid/60,mfpcafit$efunctions$level1[,1],xlab = "hour",ylab = "fpc1")
plot(binmid/60,mfpcafit$efunctions$level1[,2],xlab = "hour",ylab = "fpc2")
plot(binmid/60,mfpcafit$efunctions$level1[,3],xlab = "hour",ylab = "fpc3")
plot(binmid/60,mfpcafit$efunctions$level1[,4],xlab = "hour",ylab = "fpc4")
plot(binmid/60,mfpcafit$efunctions$level1[,5],xlab = "hour",ylab = "fpc5")
plot(binmid/60,mfpcafit$efunctions$level1[,6],xlab = "hour",ylab = "fpc6")
plot(binmid/60,mfpcafit$efunctions$level1[,7],xlab = "hour",ylab = "fpc7")
mtext("First level subject specific eigenfunctions",side=3,line=0,outer=TRUE)

par(mfrow=c(3,3))
par(oma=c(3,3,3,3),mar=c(3,4,1,1))
#level2ef
plot(binmid/60,mfpcafit$efunctions$level2[,1],xlab = "hour",ylab = "fpc1")
plot(binmid/60,mfpcafit$efunctions$level2[,2],xlab = "hour",ylab = "fpc2")
plot(binmid/60,mfpcafit$efunctions$level2[,3],xlab = "hour",ylab = "fpc3")
plot(binmid/60,mfpcafit$efunctions$level2[,4],xlab = "hour",ylab = "fpc4")
plot(binmid/60,mfpcafit$efunctions$level2[,5],xlab = "hour",ylab = "fpc5")
plot(binmid/60,mfpcafit$efunctions$level2[,6],xlab = "hour",ylab = "fpc6")
plot(binmid/60,mfpcafit$efunctions$level2[,7],xlab = "hour",ylab = "fpc7")
mtext("Second level subject_visit specific eigenfunctions",side=3,line=0,outer=TRUE)

par(mfrow=c(3,3))
par(oma=c(3,3,3,3),mar=c(3,4,1,1))
#level1ef
plot(binmid/60,mfpcafit$efunctions$level1[,1],xlab = "hour",ylab = "fpc1")
plot(binmid/60,mfpcafit$efunctions$level1[,2],xlab = "hour",ylab = "fpc2")
plot(binmid/60,mfpcafit$efunctions$level1[,3],xlab = "hour",ylab = "fpc3")
plot(binmid/60,mfpcafit$efunctions$level1[,4],xlab = "hour",ylab = "fpc4")
plot(binmid/60,mfpcafit$efunctions$level1[,5],xlab = "hour",ylab = "fpc5")
plot(binmid/60,mfpcafit$efunctions$level1[,6],xlab = "hour",ylab = "fpc6")
plot(binmid/60,mfpcafit$efunctions$level1[,7],xlab = "hour",ylab = "fpc7")
mtext("First level subject specific eigenfunctions",side=3,line=0,outer=TRUE)

par(mfrow=c(3,3))
par(oma=c(3,3,3,3),mar=c(3,4,1,1))
#level2ef
plot(binmid/60,mfpcafit$efunctions$level2[,1],xlab = "hour",ylab = "fpc1")
plot(binmid/60,mfpcafit$efunctions$level2[,2],xlab = "hour",ylab = "fpc2")
plot(binmid/60,mfpcafit$efunctions$level2[,3],xlab = "hour",ylab = "fpc3")
plot(binmid/60,mfpcafit$efunctions$level2[,4],xlab = "hour",ylab = "fpc4")
plot(binmid/60,mfpcafit$efunctions$level2[,5],xlab = "hour",ylab = "fpc5")
plot(binmid/60,mfpcafit$efunctions$level2[,6],xlab = "hour",ylab = "fpc6")
plot(binmid/60,mfpcafit$efunctions$level2[,7],xlab = "hour",ylab = "fpc7")
mtext("Second level subject_visit specific eigenfunctions",side=3,line=0,outer=TRUE)

load("mfpcafit.RData")
lev1score<-mfpcafit$scores$level1
lev2score<-mfpcafit$scores$level2
load("actmatlist_mean_visit_spec.Rdata")
load("subjdf.Rdata")
idvec<-c()
for (i in 1:92)
{idvec<-c(idvec,rep(subjdf$id[i],nrow(actmatlist[[i]])))}
##get mean and sd of level2 score for each subject
dfvisitspec<-matrix(0,92,2)
for (i in 1:92)
{ind<-which(idvec==subjdf$id[i])
tempscorev<-as.vector(lev2score[ind,])
dfvisitspec[i,1]<-mean(tempscorev)
dfvisitspec[i,2]<-sd(tempscorev)
}
subjdf<-cbind(subjdf,lev1score) ##adding level 1 scores
subjdf$education[2]<-mean(subjdf$education,na.rm=TRUE)
names(subjdf)[6:12]<-paste("level1sc",1:7,sep="")
subjdf<-subjdf[,-1]
names(subjdf)
subjdf<-cbind(subjdf,dfvisitspec) ##adding level2 scores
names(subjdf)[12:13]<-c("mean_visit","sd_visit")
glm1 <- glm(adstatus~age+Sex+education,data = subjdf,family="binomial")
fitglm_mfpca <- glm(adstatus~.,data = subjdf,family="binomial")
tab_model(glm1,fitglm_mfpca,title = "Modelling cognitive status using MFPCA of activity data")
