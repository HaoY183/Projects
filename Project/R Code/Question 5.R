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

# Question 5: 
# Does the number of vaccines have the same impact on infected cases 
# with death cases before and after when mask mandate was lifted in Indiana?

# Reading Data
covid<-read.csv("~/Desktop/covid.csv")

# Getting predictive factors:
data<-covid[c('TotalVac', 'death', 'Mask')]

model<-lm(death~TotalVac+TotalVac*factor(Mask), data)

# Checking assumptions:
# 1. Checking for constant variance
bptest(model) # Result showing for non-constant variance
# 2. Checking for normality
shapiro.test(residuals(model))
qqnorm(residuals(model))
qqline(residuals(model)) # Violation of normality assumption

# Transformation
# On X:
data$TotalVac<-log(data$TotalVac)
newModel<-lm(death~TotalVac+TotalVac*factor(Mask), data)
# Box-cox transformation on Y
bcmle<-boxcox(newModel, lambda = seq(-3,3,by=0.1))
lambda<-bcmle$x[which.max(bcmle$y)]
data$death<-data$death^lambda
newModel<-lm(death~TotalVac+TotalVac*factor(Mask), data) # new model

# Rechecking non-constant variance
bptest(newModel)
# Rechecking normality
shapiro.test(residuals(newModel))
qqnorm(residuals(newModel))
qqline(residuals(newModel)) # Improvement

summary(newModel)

# Advanced diagnostic measurement
# 1. Checking marginal effects
avPlots(newModel) # All X don't have much added-on effect

# 2. Checking for X and Y outliers
h<-2*4/116
hii<-lm.influence(newModel)$hat
length(which(hii[] > h)) # X Has 13 outliers
BonfCV<-qt(1-0.05/(2*116), 116-1-4) # Bonferroni critical value
rstudent<-rstudent(newModel)
length(which(rstudent[] > BonfCV)) # Y has no outlier ^^

# 3. Checking for influential points
dfbetas<-dfbetas(newModel)
sum(dfbetas[which(abs(dfbetas[, 2]) > 1 & abs(dfbetas[, 3])> 1 & 
                  abs(dfbetas[, 4]) > 1)]) # DFBETAS No influential point
dff<-dffits(newModel)
length(dff[dff > 1]) # DFFITS No influential point
CookDis<-cooks.distance(newModel)
minor<-qf(0.2, df1 = 4, df2 = 116 - 4)
major<-qf(0.5, df1 = 4, df2 = 116 - 4) # Two threholds
MinInf<-sum(CookDis > minor & CookDis > major)
MajInf<-sum(CookDis > major) # Cook's distance No influential point
influencePlot(newModel)

# Multicollinearity remedial: Ridge Regression
RModel<-lmridge(death~TotalVac+TotalVac*factor(Mask), data, K=seq(0,1,0.02))
# plot(RModel)
vif(RModel) # 0.06
RModel<-lmridge(death~TotalVac+TotalVac*factor(Mask), data, K=0.06)
summary(RModel) 
# The result shows that there is still multicollinearity between X1 and X2. 

# Bootstrapping
boot.ridgecoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  colnames(data)<-c('x1', 'y', 'x2')
  mod <- lmridge(y~x1+x1*factor(x2), data=data, K=0.06)
  return(coef(mod))
}
RModel<-boot(data = data, statistic = boot.ridgecoef, R=100, maxit=100)
boot.ci(RModel, index = 2, type="perc")
boot.ci(RModel, index = 3, type="perc")
boot.ci(RModel, index = 4, type="perc")

#K-Folder
set.seed(123)
train.control<-trainControl(method = 'cv', number = 5)
step.model1<-train(death~TotalVac+TotalVac*factor(Mask), data = data, 
                   method="leapBackward", tuneGrid = data.frame(nvmax = 4), 
                   trControl = train.control)
step.model1$results



