data=read.csv("diabetes.csv")
data1=read.csv("diabetes.csv",na.strings='0')

View(data)
data[1:20,]
data[1:20,]

data1[1:20,]
data1[1]=data[1]

##zero's replaced with NA's
View(data1)
data1$Outcome=data$Outcome
data1[1:20,]

##cleaning of data

##data set 1 (removing NA's)
data_nonull=data1[complete.cases(data1),]
View(data_nonull)

datano=data[data$Outcome==0,]
datayes=data[data$Outcome==1,]

datano_null=datano[complete.cases(datano),]
datayes_null=datayes[complete.cases(datayes),]
View(datano_null)

##data set 2 (Replace with mean values)
temp1=data$Insulin<1
temp2=data$Outcome==1

data_nullmean=data
data_nullmean[temp1&temp2,]$Insulin=mean(datayes_null[,5])
data_nullmean[temp1&!temp2,]$Insulin=mean(datano_null[,5])

temp1=data$Glucose==0
data_nullmean[temp1&temp2,]$Glucose=mean(datayes_null[,2])
data_nullmean[temp1&!temp2,]$Glucose=mean(datano_null[,2])

temp1=data$BloodPressure==0
data_nullmean[temp1&temp2,]$BloodPressure=mean(datayes_null[,3])
data_nullmean[temp1&!temp2,]$BloodPressure=mean(datano_null[,3])

temp1=data$SkinThickness==0
data_nullmean[temp1&temp2,]$SkinThickness=mean(datayes_null[,4])
data_nullmean[temp1&!temp2,]$SkinThickness=mean(datano_null[,4])

temp1=data$BMI==0
data_nullmean[temp1&temp2,]$BMI=mean(datayes_null[,6])
data_nullmean[temp1&!temp2,]$BMI=mean(datano_null[,6])

View(data_nullmean)

data_nonull1=data_nonull[data_nonull$Outcome==1,]
data_nonull0=data_nonull[data_nonull$Outcome==0,]
#View(data_nonull)

##data set 1 (Removed NA's)
par(mfrow=c(1,2))
boxplot(BMI~Pregnancies,data=data_nonull0,main="Pregnancies vs BMI(No Diabetes)",
        xlab="Pregnancies",ylab="Body Mass Index",col="blue")
boxplot(BMI~Pregnancies,data=data_nonull1,main="Pregnancies vs BMI(Diabetes)",
        xlab="Pregnancies",ylab="Body Mass Index",col="red")
par(mfrow=c(1,1))

boxplot(DiabetesPedigreeFunction~Outcome,data=data_nonull,xlab="Diabetes",
        ylab="Pedigree Function",main="Chances of Diabetes by Inheritance",col="green")

#par(mfrow=c(1,1))

#boxplot(Age~Outcome,data=data_nonull,xlab="Diabetes",
#        ylab="Age",main="Age vs Diabetes",col="green")

par(mfrow=c(1,2))
boxplot(BloodPressure~Glucose,data=data_nonull0,main="Glucose vs Blood Pressure(No Diabetes)",
        xlab="Glucose",ylab="Blood pressure",col="blue")
boxplot(BloodPressure~Glucose,data=data_nonull1,main="Glucose vs Blood Pressure(Diabetes)",
        xlab="Glucose",ylab="Blood pressure",col="red")




##data set 2 (Replaced with mean)

data_nullmean1=data_nullmean[data_nullmean$Outcome==1,]
data_nullmean0=data_nullmean[data_nullmean$Outcome==0,]

par(mfrow=c(1,2))
boxplot(BMI~Pregnancies,data=data_nullmean0,main="Pregnancies vs BMI(No Diabetes)",
        xlab="Pregnancies",ylab="Body Mass Index",col="blue")
boxplot(BMI~Pregnancies,data=data_nullmean1,main="Pregnancies vs BMI(Diabetes)",
        xlab="Pregnancies",ylab="Body Mass Index",col="red")
par(mfrow=c(1,1))

boxplot(DiabetesPedigreeFunction~Outcome,data=data_nullmean,xlab="Diabetes",
        ylab="Pedigree Function",main="Chances of Diabetes by Inheritance",col="green")

#par(mfrow=c(1,1))

#boxplot(Age~Outcome,data=data_nullmean,xlab="Diabetes",
#        ylab="Age",main="Age vs Diabetes",col="green")

par(mfrow=c(1,2))
boxplot(BloodPressure~Glucose,data=data_nullmean0,main="Glucose vs Blood Pressure(No Diabetes)",
        xlab="Glucose",ylab="Blood pressure",col="blue")
boxplot(BloodPressure~Glucose,data=data_nullmean1,main="Glucose vs Blood Pressure(Diabetes)",
        xlab="Glucose",ylab="Blood pressure",col="red")

## dataset 1(NA's Removed) - logistic regression

train_data=data_nonull[0:300,]
test_data=data_nonull[301:392,]
lm.fit1=glm(Outcome~.,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,92)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)



lm.fit1=glm(Outcome~Glucose+BMI+DiabetesPedigreeFunction+Pregnancies+Age,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,92)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~Glucose+BMI+DiabetesPedigreeFunction,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,92)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~Glucose+BMI,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,92)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~DiabetesPedigreeFunction+Pregnancies+Glucose+Age,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,92)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

library(ggplot2)
ggplot(test_data, aes(x=DiabetesPedigreeFunction+Pregnancies+Glucose+Age, y=Outcome)) + geom_point() + 
 stat_smooth(method="glm", family="binomial", se=FALSE)

##Dataset 2 (Replaced with mean) - logistic regression

data_nullmean1=data_nullmean[data_nullmean$Outcome==1,]
data_nullmean0=data_nullmean[data_nullmean$Outcome==0,]

train_data=data_nullmean[0:650,]
test_data=data_nullmean[651:768,]

#library(ggplot2)
#ggplot(data_nullmean, aes(x=Pregnancies, y=Outcome)) + geom_point() + 
#  stat_smooth(method="glm", family="binomial", se=FALSE)



lm.fit1=glm(Outcome~.,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,118)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~Glucose+BMI+DiabetesPedigreeFunction+Pregnancies,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,118)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~Glucose+BMI+DiabetesPedigreeFunction+Pregnancies,data=train_data,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,118)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

lm.fit1=glm(Outcome~Glucose+Insulin+Age,data=data_nullmean,family=binomial)
summary(lm.fit1)
lm.prob=predict(lm.fit1,test_data,type="response")
lm.pred=rep(0,118)
lm.pred[lm.prob>0.5]=1
table(lm.pred,test_data$Outcome)
mean(lm.pred!=test_data$Outcome)

library(ggplot2)
ggplot(test_data, aes(x=DiabetesPedigreeFunction+Pregnancies+Glucose+Age, y=Outcome)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

## dataset 1 -LDA

train_data=data_nonull[0:300,]
test_data=data_nonull[301:392,]

library(MASS)
ld.fit=lda(Outcome~.,train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)

ld.fit=lda(Outcome~Glucose+BMI+DiabetesPedigreeFunction+Pregnancies,train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)

ld.fit=lda(Outcome~Glucose+DiabetesPedigreeFunction+Pregnancies+Age,train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)






##dataset 2 -LDA

train_data=data_nullmean[0:650,]
test_data=data_nullmean[651:768,]
library(MASS)
ld.fit=lda(Outcome~.,data=train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)

ld.fit=lda(Outcome~Glucose+BMI+DiabetesPedigreeFunction+Pregnancies,data=train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)

ld.fit=lda(Outcome~Glucose+Insulin+Age,data=train_data)
ld.fit
ld.pred=predict(ld.fit,test_data,type="response")
table(ld.pred$class,test_data$Outcome)
mean(ld.pred$class!=test_data$Outcome)
