library(car)
library(carData)
library(zoo)
library(MASS)
library(lmtest)
library(boot)
library(fmsb)
library(leaps)
library(caret)
library(lmridge)

covid <- read.csv("C:/Users/74406/Desktop/covid_project/covid.csv", stringsAsFactors=TRUE)
model<-lm(infection~Doseone+Series+death, covid)
avPlots(model)
new<-subset(covid, select = -c(TotalVac, Booster, Mask))
plot(new)
res<-residuals(model)
shapiro.test(res)
qqnorm(res)
qqline(res)
bptest(model)
#logx trans
new$Doseone<-log(new$Doseone)
new$Series<-log(new$Series)
new$death<-log(new$death)
modelNew<-lm(infection~Doseone+Series+death, new)

#box-cox trans
bc<-boxcox(modelNew, lambda = seq(-3,3,by=0.1))
lambda<-bc$x[which.max(bc$y)]
new$newY<-new$infection^lambda
lambda
modelNew<-lm(newY~Doseone+Series+death, new)
new<-new[,-3]
#diagnostic
plot(new)
avPlots(modelNew)
res<-residuals(modelNew)
shapiro.test(res)
qqnorm(res)
qqline(res)
bptest(modelNew)
#advanced diagnostic
standard = 2*(4/116)
infl<-lm.influence(modelNew)$hat
length(which(infl[] > standard))
d<-dfbetas(modelNew)
dfbetasPlots(modelNew)
d[which(abs(d[,2])>1 & abs(d[,3])>1 & abs(d[,4]) > 1)]
dfbetasPlots(modelNew)
dff<-dffits(modelNew)
length(dff[dff>1])
influencePlot(modelNew)
plot(lm(newY~Doseone+Series+death, new), pch = 18, col="red", which = c(4))
qf(0.2,4,112-4)
qf(0.5,4,112-4)
VIF(lm(Doseone~Series+death, new))
VIF(lm(Series~Doseone+death, new))
VIF(lm(death~Doseone+Series, new))

#Robust
modelR<- rlm(newY~Doseone+Series+death, data = new, psi = psi.bisquare)
summary(modelR)
#bootstrapping on Robust

boot.R<-function(data, indices, maxit = 100){
  data<-data[indices,]
  mod<-rlm(newY~Doseone+Series+death, data = data, maxit=maxit)
  return(coefficients(mod))
}
modelBR<-boot(data=new,statistic = boot.R, R=100, maxit=100)
boot.ci(modelBR, index = 2, type="perc")
boot.ci(modelBR, index = 3, type="perc")
#Ridge
rid<-lmridge(newY~Doseone+Series+death, data = new, K = seq(0,1,0.02))
plot(rid)
vif(rid)
summary(lmridge(newY~Doseone+Series+death,data = new, K = 0.14))
#bootstrapping on Ridge
boot.Rid<-function(data, indices, maxit = 100){
  data<-data[indices,]
  mod<-lmridge(newY~Doseone+Series+death, data = data, maxit=maxit, K = 0.14)
  return(coefficients(mod))
}
modelrid<-boot(data=new,statistic = boot.Rid, R=100, maxit=100)
boot.ci(modelrid, index = 2, type="perc")
boot.ci(modelrid, index = 3, type="perc")

#k-fold for prediction
set.seed(123)
train.control<-trainControl(method = 'cv', number = 5)
step.model1<-train(newY~Doseone+Series+death, data = new, method="leapBackward", tuneGrid = data.frame(nvmax = 4), trControl = train.control)
step.model1$results

#without advanced method
reduced<-lm(newY~Doseone+death, new)
full<-lm(newY~Doseone+Series+death, new)
MSR<-(sum(reduced$residuals^2) - sum(full$residuals^2))/ (reduced$df.residual - full$df.residual)
MSE<-sum(full$residuals^2) / full$df.residual
FS = MSR/MSE
qf(0.95, reduced$df.residual-full$df.residual, full$df.residual)
p = 1-pf(FS,reduced$df.residual-full$df.residual, full$df.residual)
FS
p
summary(full)
cv = qt(0.975, 112)
CI_lower = -.010661 - 0.003583*cv
CI_higher = -.010661 + 0.003583*cv
CI_higher
CI_lower
DCl = 0.010766 + 0.004326*cv
DCh = 0.010766 - 0.004326*cv
DCl
DCh
#Bootstrapping on OLS
boot.ols<-function(data, indices, maxit = 100){
  data<-data[indices,]
  data.mod1<-lm(newY~Doseone+Series+death, data = data, maxit = 100)
  return(coef(data.mod1))
}
modelols<-boot(data = new, statistic = boot.ols, R = 100, maxit = 100)
boot.ci(modelols, index = 2, type="perc")
boot.ci(modelols, index = 3, type="perc")
#WLS
wts<-1/fitted(lm(abs(residuals(full))~Doseone+Series+death, new))^2
modelM<-lm(newY~Doseone+Series+death, weights = wts,data = new)
summary(modelM)

#bootstrapping on WLS
boot.wls<-function(data, indices, maxit = 100){
  data<-data[indices,]
  data.mod1<-lm(newY~Doseone+Series+death, data = data, maxit = maxit)
  wts<-1/fitted(lm(abs(residuals(full))~Doseone+Series+death, data))^2
  data.mod2<-lm(newY~Doseone+Series+death, weights = wts, data = data)
  return(coef(data.mod2))
}
modelwls<-boot(data = new, statistic = boot.wls, R = 100, maxit = 100)
boot.ci(modelwls, index = 2, type="perc")
boot.ci(modelwls, index = 3, type="perc")
