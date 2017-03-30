#Submitted by Yashaswini Huskur Lingarajaiah
#Read the Blog Train csv Data for basic feature from Column 51 to 60 including
Train<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_train.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(51:60,281)]

#visualization of data for any Preprocessing
boxplot(Train,las=2)

sapply(Train, function(x) sum(is.na(x))) 
sapply(Train, function(x) length(unique(x))) 
library(Amelia) 
missmap(Train, main = "Missing values vs observed on Basic feature Train Data")

#Read the Blog Basic feature Test csv Data, 2 from Feb and 2 from rest
Test1<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.02.03.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(51:60,281)]

#visualization of data for any Preprocessing
sapply(Test1, function(x) sum(is.na(x))) 
sapply(Test1, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test1, main = "Missing values vs observed on Basic feature Test1 Data")

Test2<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.02.27.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(51:60,281)]

#visualization of data for any Preprocessing
sapply(Test2, function(x) sum(is.na(x))) 
sapply(Test2, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test2, main = "Missing values vs observed on Basic feature Test2 Data")

Test3<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.03.14.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(51:60,281)]

#visualization of data for any Preprocessing

sapply(Test3, function(x) sum(is.na(x))) 
sapply(Test3, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test3, main = "Missing values vs observed on Basic feature Test3 Data")

Test4<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.03.25.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(51:60,281)]

#visualization of data for any Preprocessing
sapply(Test4, function(x) sum(is.na(x))) 
sapply(Test4, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test4, main = "Missing values vs observed on Basic feature Test4")

pairs(Train[1:10], main = "Basic Features Train data", pch = 21, bg = c("blue")[unclass(Train$V281)])

#linear model on Basic Features
Linear<-lm(formula=V281~V51+V52+V53+V54+V55+V56+V57+V58+V59+V60, data=Train)
plot(Train$V281, residuals(Linear))
plot(fitted.values(Linear), residuals(Linear))

print (paste('mean of Basic feature Train data',mean(Train$V281)))

print ("Summary of Basic feature Train Linear Model")
print (summary(Linear))

print ('Mean Square Error of Basic Feature Train data')
print(sum(residuals(Linear)^2))

#Validating on Basic feature test data
coefArray <- coefficients(Linear)

LinearModel <- function(Test)
{
	testResult <- coefArray[1] + coefArray[2]*Test[,1] + coefArray[3]*Test[,2] + coefArray[4]*Test[,3] + coefArray[5]*Test[,4] + coefArray[7]*Test[,6] + coefArray[8]*Test[,7] + coefArray[9]*Test[,8] + coefArray[10]*Test[,9]
	return (testResult - Test[,11])
}

#or

#For test Basic features data of February
predict1<-predict(Linear,Test1[1:10])

Actual<-mean(Test1$V281)
print (paste('Mean of Basic feature Test1 Data',Actual))

Expected<-mean(predict1)
print (paste('Mean of Basic feature predicted Test1 Data',Expected))

accuracy1<-((Actual-Expected)/Actual)
print (paste('Prediction Accuracy of Basic feature Test1 Data',1-accuracy1))

print ('Error in prediction by using Mean squared Error on Basic feature Train1 data')
print(sum((predict1-Test1[11])^2))

#For test Basic features data of February
predict2<-predict(Linear,Test2[1:10])

Actual<-mean(Test2$V281)
print (paste('Mean of Basic feature Test2 Data',Actual))

Expected<-mean(predict2)
print (paste('Mean of Basic feature predicted Test2 Data',Expected))

accuracy2<-((Actual-Expected)/Actual)
print (paste('Prediction Accuracy of Basic feature Test2 Data',1-accuracy2))

print ('Error in prediction by using Mean squared Error on Basic feature Train2 data')
print(sum((predict2-Test2[11])^2))

#For test Basic features data of March 
predict3<-predict(Linear,Test3[1:10])

Actual<-mean(Test3$V281)
print (paste('Mean of Basic feature Test3 Data',Actual))

Expected<-mean(predict3)
print (paste('Mean of Basic feature predicted Test3 Data',Expected))

accuracy3<-((Actual-Expected)/Actual)
print (paste('Prediction Accuracy of Basic feature Test3 Data',1-accuracy3))

print ('Error in prediction by using Mean squared Error on Basic feature Train3 data')
print(sum((predict3-Test3[11])^2))

#For test Basic features data of March 
predict4<-predict(Linear,Test4[1:10])

Actual<-mean(Test4$V281)
print (paste('Mean of Basic feature Test4 Data',Actual))

Expected<-mean(predict4)
print (paste('Mean of Basic feature predicted Test4 Data',Expected))

accuracy2<-((Actual-Expected)/Actual)
print (paste('Prediction Accuracy of Basic feature Test4 Data',1-accuracy2))

print ('Error in prediction by using Mean squared Error on Basic feature Train4 data')
print (sum((predict4-Test4[11])^2))

# Binomial Logistic Regression on Basic feature

#LogitTarget
#Building binomial target and on Basic feature, Assuming greaterthan 0 as 1 on train and test data
for( i in 1 : nrow(Train))
{
	if(Train$V281[i]>0)
	{
		Train$V281[i]<-1
	}
	else
	{
		Train$V281[i]<-0
	}
}

for( i in 1 : nrow(Test1))
{
	if(Test1$V281[i]>0)
	{
		Test1$V281[i]<-1
	}
	else
	{
		Test1$V281[i]<-0
	}
}

for( i in 1 : nrow(Test2))
{
	if(Test2$V281[i]>0)
	{
		Test2$V281[i]<-1
	}
	else
	{
		Test2$V281[i]<-0
	}
}

for( i in 1 : nrow(Test3))
{
	if(Test3$V281[i]>0)
	{
		Test3$V281[i]<-1
	}
	else
	{
		Test3$V281[i]<-0
	}
}

for( i in 1 : nrow(Test4))
{
	if(Test4$V281[i]>0)
	{
		Test4$V281[i]<-1
	}
	else
	{
		Test4$V281[i]<-0
	}
}

#displaying Linear relationship on each datas
pairs(Train[1:10], main = "Basic Features Train data", pch = 21, bg = c("red", "blue")[unclass(Train$V281)])

#Apply Logistic Regression on Basic feature Train dataset
Logit<-glm(formula=V281~V51+V52+V53+V54+V55+V56+V57+V58+V59+V60, family=binomial(link='logit'), data=Train)

print ('Summary of Logit on Basic features')
print (summary(Logit))

print ('Mean Square Error of Basic feature Train data')
print(sum(residuals(Logit)^2))

anova(Logit, test="Chisq")
library(pscl)
print(pR2(Logit))

Test1.result <-predict(Logit,Test1[1:10])

#Calculating Mean square error on Base Featured Test1 data
print ("Mean Square Error of Basic feature Test1 data")
print (sum((Test1.result-Test1[11])^2))

#Identifying Accuracy due to misclasification
Test1.result <- ifelse(Test1.result > 0.5,1,0)

misClasificError1 <- mean(Test1.result != Test1$V281)
print(paste('Accuracy of Basic feature Test1 data',1-misClasificError1))

Test2.result <-predict(Logit,Test2[1:10])

#Calculating Mean square error on Base Featured Test2 data
print (paste('Mean Square Error of Basic feature Test2 data',sum((Test2.result-Test2[11])^2)))

#Identifying Accuracy due to misclasification
Test2.result <- ifelse(Test2.result > 0.5,1,0)

misClasificError2 <- mean(Test2.result != Test2$V281)
print(paste('Accuracy of Basic feature Test2 data',1-misClasificError2))

Test3.result <-predict(Logit,Test3[1:10])

#Calculating Mean square error on Base Featured Test3 data
print ("Mean Square Error of Basic feature Test3 data") 
print(sum((Test3.result-Test3[11])^2))

#Identifying Accuracy due to misclasification
Test3.result <- ifelse(Test3.result > 0.5,1,0)

misClasificError3 <- mean(Test3.result != Test3$V281)
print(paste('Accuracy of Basic feature Test3 data',1-misClasificError3))

Test4.result <-predict(Logit,Test4[1:10])

#Calculating Mean square error on Base Featured Test4 data
print ("Mean Square Error of Basic feature Test4 data")
print (sum((Test4.result-Test4[11])^2))

#Identifying Accuracy due to misclasification
Test4.result <- ifelse(Test4.result > 0.5,1,0)

misClasificError4 <- mean(Test4.result != Test4$V281)
print(paste('Accuracy of Basic feature Test4 data',1-misClasificError4))

#CASE 2

#Read the Blog Train csv Data for Textual feature

Train<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_train.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(63:262,281)]

#visualization of Textual feature data for any Preprocessing
boxplot(Train,las=2)

sapply(Train, function(x) sum(is.na(x))) 
sapply(Train, function(x) length(unique(x))) 
library(Amelia) 
missmap(Train, main = "Missing values vs observed on Textual feature Train")

#Read the Blog Basic feature Test csv Data, 2 from Feb and 2 from rest
Test1<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.02.03.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(63:262,281)]

#visualization of data for any Preprocessing
sapply(Test1, function(x) sum(is.na(x))) 
sapply(Test1, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test1, main = "Missing values vs observed on Textual feature Test1")

Test2<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.02.27.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(63:262,281)] 

#visualization of data for any Preprocessing
sapply(Test2, function(x) sum(is.na(x))) 
sapply(Test2, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test2, main = "Missing values vs observed on Textual feature Test2")

Test3<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.03.14.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(63:262,281)]

#visualization of data for any Preprocessing

sapply(Test3, function(x) sum(is.na(x))) 
sapply(Test3, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test3, main = "Missing values vs observed on Textual feature Test3")

Test4<-read.csv(file = '/Users/yashuvinay/Desktop/R/Project/blogData_test-2012.03.25.00_00.csv',header=FALSE,na.strings=c(""),sep = ",",)[,c(63:262,281)] 

#visualization of data for any Preprocessing
sapply(Test4, function(x) sum(is.na(x))) 
sapply(Test4, function(x) length(unique(x))) 
library(Amelia) 
missmap(Test4, main = "Missing values vs observed on Textual feature Test4")

#linear model
Linear<-lm(formula=V281~., data=Train)
plot(Train$V281, residuals(Linear))
plot(fitted.values(Linear), residuals(Linear))

print ('Summary of Textual feature Train Linear Model')
print (summary(Linear))

#Validating on test data
predict1<-predict(Linear,Test1[1:200])

Actual<-mean(Test1$V281)
print (paste('Mean of Textual feature Test1 Data',Actual))

Expected<-mean(predict1)
print (paste('Mean of Textual feature predicted Test1 Data',Expected))

mistake1<-(Actual-Expected)/Actual
print (paste('Prediction Accuracy of Basic feature Test1 Data',1-mistake1))

print (paste('Error in prediction by using Mean squared Error on Textual feature Train1 data', sum((predict1-Test1[201])^2)))

predict2<-predict(Linear,Test2[1:200])

Actual<-mean(Test2$V281)
print (paste('Mean of Textual feature Test2 Data',Actual))

Expected<-mean(predict2)
print (paste('Mean of Textual feature predicted Test2 Data',Expected))

mistake2<-(Actual-Expected)/Actual
print (paste('Prediction Accuracy of Basic feature Test2 Data',1-mistake2))

print (paste('Error in prediction by using Mean squared Error on Textual feature Train2 data', sum((predict2-Test2[201])^2)))

predict3<-predict(Linear,Test3[1:200])

Actual<-mean(Test3$V281)
print (paste('Mean of Textual feature Test3 Data',Actual))

Expected<-mean(predict3)
print (paste('Mean of Textual feature predicted Test3 Data',Expected))

mistake3<-(Actual-Expected)/Actual
print (paste('Prediction Accuracy of Basic feature Test3 Data',1-mistake3))

print ("Error in prediction by using Mean squared Error on  Textual feature Train3 data")
print (sum((predict3-Test3[201])^2))

predict4<-predict(Linear,Test4[1:200])

Actual<-mean(Test4$V281)
print (paste('Mean of Textual feature Test4 Data',Actual))

Expected<-mean(predict4)
print (paste('Mean of Textual feature predicted Test4 Data',Expected))

mistake4<-(Actual-Expected)/Actual
print (paste('Prediction Accuracy of Basic feature Test4 Data',1-mistake4))

print (paste('Error in prediction by using Mean squared Error on Textual feature Train4 data',sum((predict4-Test4[201])^2)))

# Binomial Logistic Regression on Textual feature

#LogitTarget
#Building binomial target, Assuming greaterthan 0 as 1
for( i in 1 : nrow(Train))
{
	if(Train$V281[i]>0)
	{
		Train$V281[i]<-1
	}
	else
	{
		Train$V281[i]<-0
	}
}

for( i in 1 : nrow(Test1))
{
	if(Test1$V281[i]>0)
	{
		Test1$V281[i]<-1
	}
	else
	{
		Test1$V281[i]<-0
	}
}

for( i in 1 : nrow(Test2))
{
	if(Test2$V281[i]>0)
	{
		Test2$V281[i]<-1
	}
	else
	{
		Test2$V281[i]<-0
	}
}

for( i in 1 : nrow(Test3))
{
	if(Test3$V281[i]>0)
	{
		Test3$V281[i]<-1
	}
	else
	{
		Test3$V281[i]<-0
	}
}

for( i in 1 : nrow(Test4))
{
	if(Test4$V281[i]>0)
	{
		Test4$V281[i]<-1
	}
	else
	{
		Test4$V281[i]<-0
	}
}

Logit<-glm(formula=V281~., family=binomial(link='logit'), data=Train)

print ('Summary of Logit on Textual features')
print(summary(Logit))

print ('Mean Square Error of Textual feature Train data')
print (sum(residuals(Logit)^2))

Test1.result <-predict(Logit,Test1[1:200])

#Calculating Mean square error on Textual Featured
print ('Mean Square Error of Textual feature Test1 data')
print (sum((Test1.result-Test1[201])^2))

#Identifying Accuracy due to misclasification
Test1.result <- ifelse(Test1.result > 0.5,1,0)

misClasificError1 <- mean(Test1.result != Test1$V281)
print(paste('Accuracy of Textual feature Test1 data',1-misClasificError1))

Test2.result <-predict(Logit,Test2[1:200])

#Calculating Mean square error on Textual Featured
print ('Mean Square Error of Textual feature Test2 data')
print (sum((Test2.result-Test2[201])^2))

#Identifying Accuracy due to misclasification
Test2.result <- ifelse(Test2.result > 0.5,1,0)

misClasificError2 <- mean(Test2.result != Test2$V281)
print(paste('Accuracy of Textual feature Test2 data',1-misClasificError2))

Test3.result <-predict(Logit,Test3[1:200])

#Calculating Mean square error on Textual Featured
print ('Mean Square Error of Textual feature Test3 data')
print (sum((Test3.result-Test3[201])^2))

#Identifying Accuracy due to misclasification
Test3.result <- ifelse(Test3.result > 0.5,1,0)

misClasificError3 <- mean(Test3.result != Test3$V281)
print(paste('Accuracy of Textual feature Test3 data',1-misClasificError3))

Test4.result <-predict(Logit,Test4[1:200])

#Calculating Mean square error on Textual Featured
print ('Mean Square Error of Textual feature Test4 data')
print (sum((Test4.result-Test4[201])^2))

#Identifying Accuracy due to misclasification
Test4.result <- ifelse(Test4.result > 0.5,1,0)

misClasificError4 <- mean(Test4.result != Test4$V281)
print(paste('Accuracy of Textual feature Test4 data',1-misClasificError4))









