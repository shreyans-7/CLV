data= read.csv(choose.files())
options(scipen = 11)
# backup of data
bckdata= data
str(data)
summary(data)
#removing insignficant variables
data=data[,-1]
colnames(data)[colnames(data)=="Customer.Lifetime.Value"]="CLV"
#detecting outliers
quantile(data$CLV,seq(0,1,.05))
quantile(data$CLV,seq(0.9,1,.01))
table(data$CLV>36000)
quantile(data$CLV,seq(0.95,1,.005))
boxplot(data$CLV)
data=data[data$CLV<36000,]
boxplot(data$CLV)
data=data[data$CLV<20000,]
boxplot(data$CLV)
data=data[data$CLV<14800,]
quantile(data$CLV,seq(0.9,1,.01))
boxplot(data$CLV)
#missing values
apply(data,2,function(x) sum(is.na(x)))
#data exploration
hist(data$CLV)
plot(data$CLV~data$Income)
boxplot(data$CLV~data$Gender)
plot(data$CLV~data$Number.of.Policies)
plot(data$CLV~data$Vehicle.Class)
plot(data$CLV~data$Sales.Channel)
plot(data$CLV~data$Monthly.Premium.Auto)
plot(data$CLV~data$State)
plot(data$CLV~data$Effective.To.Date)
data= data[,!colnames(data) %in%  c("State","Effective.To.Date")]
table(data$Number.of.Policies)
str(data$Number.of.Policies)
data$Number.of.Policies=as.factor(data$Number.of.Policies)
str(data)


#split data set 
library(caTools)
nrow(data)
nrow(bckdata)
#amt of data loss
(nrow(bckdata)-nrow(data))

set.seed(11)
smpl= sample.split(data$CLV,0.75)
train.d= subset(data,smpl==T)
test.d = subset(data,smpl==F)


# model Building
attach(train.d)

m1= lm(CLV~.,data = train.d)
summary(m1)

m2=lm(CLV~ Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
        I(EmploymentStatus== "Unemployed")  + Gender + Income + Marital.Status  
      + Monthly.Premium.Auto +  Months.Since.Policy.Inception + Number.of.Open.Complaints + 
        Number.of.Policies+ Sales.Channel +Total.Claim.Amount+ Vehicle.Class  , data = train.d )
summary(m2)

m3= lm(CLV~   Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
         I(EmploymentStatus== "Unemployed")  + Gender + Income + I(Marital.Status=="Married")  
       + Monthly.Premium.Auto +  Months.Since.Policy.Inception + Number.of.Open.Complaints + 
         Number.of.Policies+ Sales.Channel +Total.Claim.Amount+ Vehicle.Class   , data = train.d )
summary(m3)

m4=lm(CLV~ Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
        I(EmploymentStatus== "Unemployed")  + Gender + Income + I(Marital.Status=="Married")  
      + Monthly.Premium.Auto +  Months.Since.Policy.Inception + Number.of.Open.Complaints + 
        Number.of.Policies+ Sales.Channel + Vehicle.Class   , data = train.d )

summary(m4)

m5 = lm(CLV~ Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
          I(EmploymentStatus== "Unemployed")  + Gender + Income + I(Marital.Status=="Married")  
        + Monthly.Premium.Auto +  Months.Since.Policy.Inception + Number.of.Open.Complaints + 
          Number.of.Policies + I(Vehicle.Class=="Sports Car")+
          I(Vehicle.Class=="SUV") , data = train.d )
summary(m5)

#check multicollinearity
library(car)
vif(m5)

#fitted model & mape
fitted(m5)
train.d$fit=fitted(m5)

library(Metrics)
mape(train.d$CLV,train.d$fit)

#normality of errors
plot(density(m5$residuals))
library(nortest)
ad.test(m5$residuals)

par(mfrow=c(2,2))
plot(m5)

pearson.test(m5$residuals)
par(mfrow=c(1,1))
#homoscedasticity
library(lmtest)
bptest(m5)

ncvTest(m5)
plot(m5$residuals)

#auto correlation
durbinWatsonTest(m5)


#testing on train dataset
t1= lm(CLV~ Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
         I(EmploymentStatus== "Unemployed")  + Gender + Income + I(Marital.Status=="Married")  
       + Monthly.Premium.Auto +  Months.Since.Policy.Inception + Number.of.Open.Complaints + 
         Number.of.Policies + I(Vehicle.Class=="Sports Car")+
         I(Vehicle.Class=="SUV") , data = test.d )
summary(t1)

t2 = lm(CLV~   Coverage + Education  + I(EmploymentStatus== "Employed" )  + 
          I(EmploymentStatus== "Unemployed")  + Gender + Income + I(Marital.Status=="Married")  
        + Monthly.Premium.Auto  + Number.of.Open.Complaints + 
          Number.of.Policies + I(Vehicle.Class=="Sports Car")+
          I(Vehicle.Class=="SUV") , data = test.d)

summary(t2)

t3= lm(CLV~   Coverage + I(Education=="Doctor")  + I(Education=="Master")  + I(EmploymentStatus== "Employed" )  + 
         I(EmploymentStatus== "Unemployed")   + Income + I(Marital.Status=="Married")  
       + Monthly.Premium.Auto  + Number.of.Open.Complaints + 
         Number.of.Policies + I(Vehicle.Class=="Sports Car")+
         I(Vehicle.Class=="SUV") , data = test.d)

summary(t3)

t4= lm(CLV~   Coverage + I(Education=="Doctor")  + I(Education=="Master")  + I(EmploymentStatus== "Employed" )  +   
         Income + I(Marital.Status=="Married")   + Monthly.Premium.Auto  + Number.of.Open.Complaints + 
         Number.of.Policies + I(Vehicle.Class=="Sports Car")+ I(Vehicle.Class=="SUV") , data = test.d)

summary(t4)


#applying the t4 model on the training data set

v1= lm(CLV~   Coverage + I(Education=="Doctor")  + I(Education=="Master")  + I(EmploymentStatus== "Employed" )  + 
         I(EmploymentStatus== "Unemployed")   + Income + I(Marital.Status=="Married")  
       + Monthly.Premium.Auto  + Number.of.Open.Complaints + 
         Number.of.Policies + I(Vehicle.Class=="Sports Car")+
         I(Vehicle.Class=="SUV") , data = train.d)


summary(v1)

#multi collinearity
vif(t4)


#fitting of model
test.d$pred= fitted(t4,test.d)

#&MAPE

library(Metrics)
mape(test.d$CLV,test.d$pred)

#error  analysis
plot(test.d$CLV-test.d$pred)
ad.test(t4$residuals)# normality of errors
densityPlot(t4$residuals)

par(mfrow=c(2,2))
plot(t4)

par(mfrow=c(1,1))
pearson.test(t4$residuals)

#homoscedasticity
library(lmtest)
bptest(t4)

ncvTest(t4)

# auto correlation test
durbinWatsonTest(t4)


