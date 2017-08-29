library(ggplot2)
library(dummy)
library(dplyr)
library(car)
library(randomForest)
library(devtools)
getwd()
bank=read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = "unknown")
View(bank)
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
bank=bank[,-16]


##====== Character to factore ================
is.factor(bank[,2])
for(i in 1 : ncol(bank)){
if(class(bank[,i])=="character"){
  bank[,i]=as.factor(bank[,i])

}else{}

}
bank = bank[,-16]

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

bank$balance=log(bank$balance)
bank$duration=log(bank$duration)

bank$duration[is.na((bank$duration))]=8
bank$duration[is.infinite(bank$duration)]=8

bank$balance[is.na((bank$balance))]=10
bank$balance[is.infinite(bank$balance)]=10
##====== rf model ================
str(bank)


#rf on complete train data
fitrf=randomForest(y~.,data=bank)
summary(fitrf)

fitrf$confusion

##====== rf model on test and train ================
str(bank_test)

set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.75)

bank_train =bank[a,]
bank_test =bank[-a,]
set.seed(2)

# fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-score-yp,data=bank_train, ntree=200)
fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-score-yp,data=bank_train, ntree=500) #90.40698
# fitrf=randomForest(y~duration+month+ age,data=bank_train, ntree= 200)
# fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-previous-campaign-pdays-job -score-yp,data=bank_train, ntree=200)
summary(fitrf)

fitrf$importance 
varImpPlot(fitrf)
plot(fitrf)
var.imp <- data.frame(importance(fitrf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

a=getTree(fitrf, 1, labelVar=TRUE)
View(a)
as.tree(a)
install.packages
windows()
reprtree:::plot.getTree(fitrf)
devtools::install_github("araastat/reprtree")

bank_train$score =predict(fitrf,bank_train,type = "prob")[,c(2)]
bank_test$score =predict(fitrf,bank_test,type = "prob")[,c(2)]
bank_test$yp =predict(fitrf,bank_test)
a=table(bank_test$y,bank_test$score)
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
# 0.4034547
#Accuracy
d=(a[1,1]+a[2,2])/ sum(a)
d*100 

trn_y=ifelse(bank_train$y=="yes",1,0)
tst_y=ifelse(bank_test$y=="yes",1,0)
a=confmatrix(trn_y,bank_train$score,"KS",tst_y,bank_test$score)  #0.354
View(a$table)
bank_test$yps=ifelse(bank_test$score>0.354,1,0)
a1=bank_test[,c(16,17,18,19)]

hist(bank_test[bank_test$y=="yes","score"])

##====== test ================
getwd()
bank_t=read.csv("bank-full_test.csv",stringsAsFactors = F,na.strings = "unknown")
summary(bank_t)
str(bank_t)
head(bank_t)
colSums(is.na(bank_t))
unique(bank_t$job)
table(bank_t$balance)





##====== Data modification=============

bank_t$job=gsub("[[:punct:]]" , "", bank_t$job)



##====== NA removal ================


bank_t[is.na(bank_t$job),"job"] = "NAJob"                             # Creating a new job category for NA


# Removing NA in education based on job

bank_t[ is.na(bank_t$education) & bank_t$job%in%c("bluecollar","admin","management","retired"),"education"] ="tertiary"
bank_t[ is.na(bank_t$education) & bank_t$job%in%c("technician","selfemployed","services"),"education"] ="secondary"
bank_t[ is.na(bank_t$education) ,"education"] ="primary"

# Removing NA by replacing with mode

bank_t[ is.na(bank_t$contact) ,"contact"] = "cellular"

# Removing poutcome as it contains many NAs
bank_t=bank_t[,-16]


##====== Character to factore ================
str(bank_t)
is.factor(bank_t[,2])
for(i in 1 : ncol(bank_t)){
  if(class(bank_t[,i])=="character"){
    bank_t[,i]=as.factor(bank_t[,i])
    
  }else{}
  
}
bank_t = bank_t[,-16]

##====== Prediction ================

bank_t$score =predict(fitrf,bank_t,type = "prob")[,c(2)]
bank_t$y= ifelse(bank_t$score>0.354,"yes","no")
table(bank_t$y)

# cutoff=0.848
write.csv(bank_t$y, file = "2_rf.csv",row.names = F)
# no   yes 
# 11605  1959 

##========##====== KS chart ====================
str(bank_test)
bank_test$score =predict(fitrf,bank_test,type = "prob")[,c(2)]
a=bank_test[,c(16,17)]
head(a)
a$y=ifelse(a$y=="yes",1,0)
a=a[order(a$score,decreasing = F),]
a1=rep(1:10,each=791)
length(a1)
a1=c(a1,c(10,10))
a2=cbind(a,a1)
k=NULL
l=NULL
for(i in 1:10){
  a[i]=c(nrow(a2[a2$y==0 & a2$a1==i,]),nrow(a2[a2$y==1 & a2$a1==i,]))
  b[i]=c(min(a2[a2$y==0 & a2$a1==i,"score"]),max(a2[a2$y==0 & a2$a1==i,"score"]))
 if(i==10){
   k=c(k,a[1:2,1:10])
  l=c(l,b[1:2,1:10])
 }
}
k
l
a=data.frame(k)
a <- as.data.frame(t(a[]))
b=data.frame(l)
b <- as.data.frame(t(b[]))
a=cbind(a,b)
cummright=NULL
cummwrong=NULL
colnames(a)=c("zero","one","min","max")
a1=a %>% mutate(percright=(one/sum(one))*100,
                 percworng=(zero/sum(zero))*100,
                cummright<-cummright+lag(percright,default=0),
                cummwrong<-cummwrong+lag(percworng,default=0)
                
)

a1



str(bank_test)
bank_test$score =predict(fitrf,bank_test,type = "prob")[,c(2)]
a=bank_test[,c(16,17)]
head(a)
a$y=ifelse(a$y=="yes",1,0)
a=a[order(a$score,decreasing = F),]
head(a)
cummright=NULL
cummwrong=NULL
a1=a %>% mutate(percright=if(y==1){(1/length(y==1))*100},
                percworng=if(y==0){(1/length(y==0))*100}
                # cummright<-cummright+lag(percright,default=0),
                # cummwrong<-cummwrong+lag(percworng,default=0)
                
)

a1
(1/length(a$y==0))*100
a$percright[a$y==1]=(1/sum(a$y==1))*100
a$percright[a$y==0]=0
a$percworng[a$y==1]=0
a$percworng[a$y==0]=(1/sum(a$y==0))*100

str(bank_test)
bank_test$score =predict(fitrf,bank_test,type = "prob")[,c(2)]
a=bank_test[,c(16,17)]
head(a)
a$y=ifelse(a$y=="yes",1,0)
a=a[order(a$score,decreasing = F),]
head(a)
cummright=NULL
cummwrong=NULL
a1=a %>% mutate(percright=if(y==1){(1/length(y==1))*100},
                percworng=if(y==0){(1/length(y==0))*100}
                # cummright<-cummright+lag(percright,default=0),
                # cummwrong<-cummwrong+lag(percworng,default=0)
                
)

a1
(1/length(a$y==0))*100
a$percright[a$y==1]=(1/sum(a$y==1))*100
a$percright[a$y==0]=0
a$percworng[a$y==1]=0
a$percworng[a$y==0]=(1/sum(a$y==0))*100






bank_test$yp=ifelse(bank_test$score>0.404,1,0)
a=table(bank_test$y,bank_test$yp)
a
a[1,1]/sum(a[1,])
a[2,2]/sum(a[2,])
a[2,1]/sum(a[2,])



a[1,1]/sum(a[,1])
a[2,2]/sum(a[,2])

bank_train$yp=ifelse(bank_train$score>0.414,1,0)
a=table(bank_train$y,bank_train$yp)
a
a[1,1]/sum(a[1,])
a[2,2]/sum(a[2,])-a[2,1]/sum(a[2,])

a[1,1]/sum(a[,1])
a[2,2]/sum(a[,2])