### Setting Directory
setwd("C:\\Users\\vuddi\\Desktop\\DS Files\\Datasets")
#sink("SLR_Output.txt")
getwd()

### Loading Packages
install.packages("alr3")
library(alr3)

### Read Data
SLR<-read.csv("Salary_Data.csv")

### Understand the data 
head(SLR)
str(SLR)
dim(SLR)
summary(SLR)

###Find the correlation between dependent and independent variables
cor(SLR$YearsExperience,SLR$Salary)
plot(SLR)

###Build a linear regression model
SLR_model<-lm(Salary~YearsExperience, data = SLR)

###Find the summary of the model
summary(SLR_model)

####Regression Coefficients
coeff<-coefficients(SLR_model)
coeff

####Predictions
pred<-predict(SLR_model, data = SLR)
head(pred)

###Residuals
error<-(SLR$Salary-pred)
head(error)

###R-Squared
summary(SLR_model)$r.squared

##############New Prediction #####################
attach(SLR)     # attach the data frame
SLR_model<-lm(Salary~YearsExperience)
newdata = data.frame(YearsExperience=3)

###Prediction intervals
predict(SLR_model, newdata, interval="confidence")
detach(SLR)
###################################

### Check Residuals Distribution
SLR_model = lm(Salary ~ YearsExperience, data=SLR)
SLR.res = resid(SLR_model)
plot(SLR$YearsExperience, SLR.res,
     ylab="Residuals", xlab="YearsExperience", 
     main="Salary Prediction")
abline(0, 0)                  # the horizon

# Check Standard Residuals Distribution
SLR_model = lm(Salary ~ YearsExperience, data=SLR)
SLR.stdres = rstandard(SLR_model)
plot(SLR$YearsExperience, SLR.stdres,
     ylab="Standardized Residuals", 
     xlab="YearsExperience", 
     main="Salary Prediction")
abline(0, 0)                  # the horizon

# Check Standard Residuals is nOrmally Distribution using QQ Plot
SLR_model = lm(Salary ~ YearsExperience, data=SLR)
SLR.stdres = rstandard(SLR_model)
qqnorm(SLR.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Salary Prediction")
qqline(SLR.stdres)

# Check Diagnostic Plots
par(mfrow=c(2,2))
plot(SLR_model)

###Breusch-Pagan (BP) test for Homoscedasticity
install.packages("lmtest")
library(lmtest)
bptest(SLR_model)
#sink()
