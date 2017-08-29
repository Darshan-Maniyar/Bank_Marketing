library(ggplot2)
library(dummy)
library(dplyr)
# library(car)
# library(randomForest)
library(devtools)
library(e1071)
getwd()
bank=read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = "unknown")
#View(bank)
summary(bank)
str(bank)
head(bank)
colSums(is.na(bank))
unique(bank$loan)
unique(bank$age)
table(bank$balance)
attach(bank)
glimpse(bank)
##====== Data modification=============

bank$job=gsub("[[:punct:]]" , "", bank$job)



##====== NA removal ================


bank[is.na(bank$job),"job"] = "NAJob"                             # Creating a new job category for NA


# Removing NA in education based on job

bank[ is.na(bank$education) & bank$job%in%c("bluecollar","admin","management","retired"),"education"] ="tertiary"
bank[ is.na(bank$education) & bank$job%in%c("technician","selfemployed","services"),"education"] ="secondary"
bank[ is.na(bank$education) ,"education"] ="primary"

# Removing NA by replacing with mode

bank[ is.na(bank$contact) ,"contact"] = "cellular"

# Removing poutcome as it contains many NAs
bank$poutcome=NULL
bank$ID=NULL

##====== Character to factore ================
is.factor(bank[,2])
for(i in 1 : ncol(bank)){
  if(class(bank[,i])=="character"){
    bank[,i]=as.factor(bank[,i])
    
  }else{}
  
}


# ##====== stadadized int variable ================
# class(bank[,1])
# bank[,1]=((bank[,1]-mean(bank[,1]))/(sd(bank[,1])))
# bank[,6]=((bank[,6]-mean(bank[,6]))/(sd(bank[,6])))
# bank[,12]=((bank[,12]-mean(bank[,12]))/(sd(bank[,12])))
# bank[,13]=((bank[,13]-mean(bank[,13]))/(sd(bank[,13])))
# bank[,14]=((bank[,14]-mean(bank[,14]))/(sd(bank[,14])))
# bank[,15]=((bank[,15]-mean(bank[,15]))/(sd(bank[,15])))
# 
# for(i in 1 : (ncol(bank))){
#   if(bank[,i]=="integer"){
#     bank[,i]=((bank[,i]-mean(bank[,i]))/(sd(bank[,i])))
#     print(bank[,i])
#   }else{}
#   }

# ##====== log tranform int variable ================

# hist(bank$balance[bank$balance<1])
# bank$balance=log(bank$balance+1)
# bank$duration=log(bank$duration+1)

median(bank$duration)
hist(bank$duration)
bank$duration[is.na((bank$duration))]=181
# bank$duration[is.infinite(bank$duration)]=8
median(bank$balance)
hist(bank$balance)

bank$balance[is.na((bank$balance))]=445
# bank$balance[is.infinite(bank$balance)]=8
hist(pdays)
bank$is_pcont=ifelse(bank$pdays==-1,0,1)
bank$is_pcont=as.factor(bank$is_pcont)

table(bank$previous)


bank$pdays[bank$pdays== -1]=99999999999
bank$y=ifelse(bank$y=="yes",1,0)
bank$y=as.factor(bank$y)
##====== rf model ================
str(bank)

colSums(is.na(bank))


##====== rf model on test and train ================
detach(bank)

set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.7)

bank_train =bank[a,]
bank_test =bank[-a,]
str(bank_train)


set.seed(2)
model <- svm(y ~. , bank_train,kernel="sigmoid", gamma= 0.2)
summary(model)
5490/nrow(bank_train) = "linear"
3787/nrow(bank_train) #= "sigmoid" gamma= 0.1
5220/nrow(bank_train) = "radial"
5317/nrow(bank_train) = "polynomial"
str(data.frame(model$SV))

bank_train$score =predict(model,bank_train,type = "prob")#[,c(2)]
hist(bank_train$score)
a=table(bank_train$score,bank_train$y)
bank_test$score =predict(model,bank_test,type = "prob")#[,c(2)]
a=table(bank_test$score,bank_test$y)
bank_test$yp =predict(model,bank_test)

# predictedY <- predict(model, bank_test)

points(bank_test$y, predictedY, col = "red", pch=4)

a=table(bank_test$y,bank_test$yp)


TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
# 0.08744986

Acc=(a[1,1]+a[2,2])/ sum(a)
Acc*100 
# 88.62487