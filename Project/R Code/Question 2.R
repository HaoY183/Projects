#Load needed libraries
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

#Load dataset
covid = read.csv("/Users/taiyangfurenmrssun/Desktop/Academics/2023 Spring/STAT 512/Project/covid.csv")

#Build model
model = lm(infection ~ Doseone + Series + death, data = covid)
new = subset(covid, select = -c(TotalVac, Booster, Mask))

#Check for constant variance
bptest(model)

#Check for normality
shapiro.test(residuals(model))
qqnorm(residuals(model))
qqline(residuals(model))

#Transformation on x
new$Doseone = log(new$Doseone)
new$Series = log(new$Series)
new$death = log(new$death)
modelNew = lm(infection ~ Doseone + Series + death, data = new)

#Transformation on y
bc = boxcox(modelNew, lambda = seq(-3, 3,by = 0.1))
lambda = bc$x[which.max(bc$y)]
lambda
new$newY = new$infection ^ lambda
modelNew = lm(new$newY ~ Doseone + Series + death, data = new)
new = new[, -3]

#Check for constant variance after transformation
bptest(modelNew)

#Check for normality after transformation
shapiro.test(residuals(modelNew))
qqnorm(residuals(modelNew))
qqline(residuals(modelNew))

#Check for marginal effect of predictors
avPlots(modelNew)

#Check for x/y outliers
sum(abs(rstudent(modelNew)) > 2)
infl = lm.influence(modelNew)$hat
length(which(infl[] > 2 * (4 / 116)))

#Check for influential points
d = dfbetas(modelNew)
sum(d[which(abs(d[, 2]) > 1 & abs(d[, 3])> 1 & abs(d[, 4]) > 1)])
dff = dffits(modelNew)
length(dff[dff > 1])
minor = qf(0.2, df1 = 4, df2 = 116 - 4)
major = qf(0.5, df1 = 4, df2 = 116 - 4)
Cooksdistance = cooks.distance(modelNew)
sum(Cooksdistance > minor)
sum(Cooksdistance > major)

#Check for multicollinearity
VIF(lm(Doseone~Series+death, new))
VIF(lm(Series~Doseone+death, new))
VIF(lm(death~Doseone+Series, new))

#Ridge regression for multicollinearity
rid = lmridge(newY ~ Doseone + Series + death, data = new, K = seq(0, 1, 0.02))
plot(rid)
vif(rid)
summary(lmridge(newY ~ Doseone + Series + death,data = new, K = 0.14))

#Bootstrapping for nornormality, on top of ridge regression
boot.Rid = function(data, indices, maxit = 100){
  data<-data[indices,]
  mod<-lmridge(newY ~ Doseone + Series + death, data = data, maxit = maxit, K = 0.14)
  return(coefficients(mod))
}
modelrid = boot(data=new,statistic = boot.Rid, R = 100, maxit = 100)
boot.ci(modelrid, index = 2, type="perc")
boot.ci(modelrid, index = 3, type="perc")

#Reduced model(under H0)
combined = rowSums(new[, c("Doseone", "Series")])
reduced = lm(newY ~ combined + death, data = new)

#Full model(under Ha)
full = lm(newY ~ Doseone + Series + death, data = new)
summary(full)

#F test for hypothesis testing
MSR = (sum(reduced$residuals ^ 2) - sum(full$residuals ^ 2))/ (reduced$df.residual - full$df.residual)
MSE = sum(full$residuals ^ 2) / full$df.residual
FS = MSR / MSE
FS
qf(1 - 0.05, reduced$df.residual - full$df.residual, full$df.residual)
p = 1 - pf(FS, reduced$df.residual - full$df.residual, full$df.residual)
p

#K-fold cross validation to check predictability
set.seed(123)
train.control<-trainControl(method = 'cv', number = 5)
step.model1 = train(newY ~ Doseone + Series + death, data = new, method="leapBackward", tuneGrid = data.frame(nvmax = 4), trControl = train.control)
step.model1$results

