# library(ActCR)
# x is a activity vector of length 1440/window
# cos_coeff = ActCosinor(x = actvec, window = 10) #window of 10 minutes
# This extracts the cosinor parameters.
# Loading pre extracted data
path <- "E:/JHUPD/Deliverables/data/data"
setwd(path)
load("cogdfcosinor.RData")
par(mfrow=c(2,2))
boxplot(mes~adstatus,data=dfcomb,names=c("control","ad"),ylab="mes")
boxplot(amp~adstatus,data=dfcomb,names=c("control","ad"),ylab="amp")
boxplot(acr~adstatus,data=dfcomb,names=c("control","ad"),ylab="acr")

par(mfrow=c(2,2))
boxplot(mes~Sex,data=dfcomb,names=c("Female","Male"),ylab="mes")
boxplot(amp~Sex,data=dfcomb,names=c("Female","Male"),ylab="amp")
boxplot(acr~Sex,data=dfcomb,names=c("Female","Male"),ylab="acr")

par(mfrow=c(2,2))
boxplot(mes~Sex,data=dfcomb,names=c("Female","Male"),ylab="mes")
boxplot(amp~Sex,data=dfcomb,names=c("Female","Male"),ylab="amp")
boxplot(acr~Sex,data=dfcomb,names=c("Female","Male"),ylab="acr")


library(dplyr)
library(knitr)
library(kableExtra)
library(readr)
library(tidyr)
library(sjPlot)

dfcomb$education[2]<-mean(dfcomb$education,na.rm=TRUE)
glm1<-glm(adstatus~age+sex+education,data=dfcomb,family = "binomial")
glm11<-glm(adstatus~age+sex+education+mes,data=dfcomb,family = "binomial")
glm12<-glm(adstatus~age+sex+education+amp,data=dfcomb,family = "binomial")
glm13<-glm(adstatus~age+sex+education+acr,data=dfcomb,family = "binomial")
tab_model(glm1,glm11,glm12,glm13,title = " Generalized Linear Model of adstatus on age, sex, education and cosinor parametrs")

lm11<-lm(ATTN~age+sex+education,data=dfcomb)
lm12<-lm(ATTN~age+sex+education+mes,data=dfcomb)
lm13<-lm(ATTN~age+sex+education+amp,data=dfcomb)
lm14<-lm(ATTN~age+sex+education+acr,data=dfcomb)

tab_model(lm11,lm12,lm13,lm14,title = "Modelling cognitive scores of attention using cosinor parameters")

lm21<-lm(VM~age+sex+education,data=dfcomb)
lm22<-lm(VM~age+sex+education+mes,data=dfcomb)
lm23<-lm(VM~age+sex+education+amp,data=dfcomb)
lm24<-lm(VM~age+sex+education+acr,data=dfcomb)

tab_model(lm21,lm22,lm23,lm24,title = "Modelling cognitive scores of verbal memory using cosinor parameters")

lm31<-lm(ExecFunction~age+sex+education,data=dfcomb)
lm32<-lm(ExecFunction~age+sex+education+mes,data=dfcomb)
lm33<-lm(ExecFunction~age+sex+education+amp,data=dfcomb)
lm34<-lm(ExecFunction~age+sex+education+acr,data=dfcomb)
tab_model(lm31,lm32,lm33,lm34,title = "Modelling cognitive scores of executive function using cosinor parameters")