# library(ActCR)
# x is a activity vector
# raval = RA(x = x, method = "average")
# This extracts the circadian parameters.
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