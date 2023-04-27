library(lmtest)
library(MASS)
library(boot)
library(car)
library(lmridge)
library(caret)
library(fmsb)

# Define Variables
covid <- read.csv("C:/Users/Bella/Desktop/covid.csv")
subset = subset(covid, select = c(Doseone, Series, Booster, death, infection))
data <- subset[apply(subset, 1, function(row) all(row != 0)),]
x2 <- data$Doseone
x3 <- data$Series
x4 <- data$Booster
x5 <- data$death
y <- data$infection

# Define Full Model
covidFull.mod <- lm(y~x2+x3+x4+x5)
summary(covidFull.mod)

# Check Variance Constancy
bptest(covidFull.mod)

# Check Variance Normality
shapiro.test(residuals(covidFull.mod))
qqnorm(residuals(covidFull.mod))

# Check Outlier
data$outlier <- abs(rstudent(covidFull.mod)) > 2
sum(data$outlier)
data <- data[data$outlier == FALSE, ]
 
# Transform X and Y Data
newx2 <- log(data$Doseone)
newx3 <- log(data$Series)
newx4 <- log(data$Booster)
newx5 <- log(data$death)
newy <- data$infection
covidNew.mod <- lm(newy~newx2+newx3+newx4+newx5)
bcmle <- boxcox(covidNew.mod, lambda=seq(-5,5,by=0.1))
lambda <- bcmle$x[which.max(bcmle$y)]
newy <- data$infection^lambda
covidTrans.mod <- lm(newy~newx2+newx3+newx4+newx5)

# Check Constant Variance
bptest(covidTrans.mod)

# Check Normality
shapiro.test(residuals(covidTrans.mod))
qqnorm(residuals(covidTrans.mod))

# Check Marginal Effect
avPlots(covidTrans.mod)

# Check Influential Points
d = dfbetas(covidTrans.mod)
sum(d[which(abs(d[, 2]) > 1 & abs(d[, 3])> 1 & abs(d[, 4]) > 1)])
dff = dffits(covidTrans.mod)
length(dff[dff > 1])
minor = qf(0.2, df1 = 4, df2 = 76 - 4)
major = qf(0.5, df1 = 4, df2 = 76 - 4)
Cooksdistance = cooks.distance(covidTrans.mod)
sum(Cooksdistance > minor)
sum(Cooksdistance > major)
influencePlot(covidTrans.mod)

# Check Multicollinearity
pairs(data[, c("Doseone", "Series", "Booster", "death")])
VIF(lm(Doseone~Series+Booster+death, data=data))
VIF(lm(Series~Doseone+Booster+death, data=data))
VIF(lm(Booster~Doseone+Series+death, data=data))
VIF(lm(death~Doseone+Series+Booster, data=data))

# Ridge Regression
newdata <- data.frame(newx2, newx3, newx4, newx5, newy)
Ridge.mod <- lmridge(newy~newx2+newx3+newx4+newx5, data =newdata, K=seq(0,0.2,0.02))
plot(covidRidge.mod)
vif(covidRidge.mod)
covidRidge.mod <- lmridge(newy~newx2+newx3+newx4+newx5, data =newdata, K=0.12)
summary(covidRidge.mod)

# Reduced Model
combined <- log(data$Series+data$Booster)
covidReduced.mod <- lm(newy~newx2+combined+newx5)

# F test
anova(covidReduced.mod,covidTrans.mod)
Fs <- anova(covidReduced.mod,covidTrans.mod)$F[2]
Fc <- qf(1-0.05, covidReduced.mod$df.residual-covidTrans.mod$df.residual, covidTrans.mod$df.residual)

# K-fold Cross Validation
set.seed(123)
train.control <- trainControl(method = 'cv', number = 5)
step.model1 = train(newy ~ newx2 + newx3 + newx4 + newx5, data = newdata, method="leapBackward", tuneGrid = data.frame(nvmax = 4), trControl = train.control)
step.model1$results

