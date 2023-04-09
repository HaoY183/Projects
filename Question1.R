library(car)
library(carData)
library(zoo)
library(MASS)
library(lmtest)
library(boot)
library(fmsb)
library(leaps)
library(caret)

covid <- read.csv("C:/Users/74406/Desktop/stat512/covid.csv", stringsAsFactors=TRUE)
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
standard = 2*(4/117)
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
VIF(lm(Doseone~Series+death, new))
VIF(lm(Series~Doseone+death, new))
VIF(lm(death~Doseone+Series, new))

#Robust
modelR<- rlm(newY~Doseone+Series+death, data = new, psi = psi.bisquare)
summary(modelR)
#bootstrapping

boot.R<-function(data, indices, maxit = 100){
  data<-data[indices,]
  mod<-rlm(newY~Doseone+Series+death, data = data, maxit=maxit)
  return(coefficients(mod))
}
modelBR<-boot(data=new,statistic = boot.R, R=100, maxit=100)
boot.ci(modelBR, index = 2, type="perc")
boot.ci(modelBR, index = 3, type="perc")

#k-fold
set.seed(123)
train.control<-trainControl(method = 'cv', number = 10)
step.model1<-train(newY~Doseone+Series+death, data = new, method="leapBackward", tuneGrid = data.frame(nvmax = 4), trControl = train.control)
step.model1$results