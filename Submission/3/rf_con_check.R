library(ggplot2)
library(dummy)
library(dplyr)
library(car)
library(randomForest)
library(devtools)
library(plotly)
getwd()
bank=read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = "unknown")
# View(bank)
table(bank$y)
summary(bank)
str(bank)
head(bank)
colSums(is.na(bank))
unique(bank$loan)
unique(bank$age)
table(bank$balance)
attach(bank)
glimpse(bank)

bank$ID=NULL
bank$poutcome=NULL
bank$contact=NULL


##====== Data modification=============

bank$job=gsub("[[:punct:]]" , "", bank$job)



##====== NA removal ================


bank[is.na(bank$job),"job"] = "NAJob"                             # Creating a new job category for NA


# Removing NA in education based on job

bank[ is.na(bank$education) & bank$job%in%c("bluecollar","admin","management","retired"),"education"] ="tertiary"
bank[ is.na(bank$education) & bank$job%in%c("technician","selfemployed","services"),"education"] ="secondary"
bank[ is.na(bank$education) ,"education"] ="primary"



##====== Feture enginnering ================

set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.80)

bank_test =bank[-a,]
bank =bank[a,]


a=data.frame(prop.table(table(log((bank$balance+1)^2),bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()
dim(a)
ggplotly(p)


bank =bank %>%  mutate ( balance_code1 = ifelse(bank$balance<0,1,0),
                         balance_code2 = ifelse(bank$balance>=0 & bank$balance<750,1,0),
                         balance_code3 = ifelse(bank$balance>=750,1,0)
                         ) %>% select(-balance,-balance_code2)

# a
# a$balance_code1=ifelse(a$Freq<0.25,1,0)
# a$balance_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$balance_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$balance_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="balance"
# bank=merge(a,bank,by="balance")
# bank$balance=NULL
# bank$balance_code1=NULL


# a=data.frame(prop.table(table((bank$month),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# a
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# a$code=ifelse(a$Freq>0.2,1,0)
# str(a)
# a=a %>% select(Var1,code)
# names(a)[1]="month"
# names(a)[2]="month_code"
# bank=merge(a,bank,by="month")
# bank$month=NULL
# 
# str(bank)
# table(bank$month_code)


a=data.frame(prop.table(table((bank$duration),bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank$duration_code1=ifelse(bank$duration <250,1,0)
bank$duration_code2=ifelse(bank$duration >=250 & bank$duration < 500,1,0)
bank$duration_code3=ifelse(bank$duration >=500 & bank$duration < 600,1,0)
bank$duration_code4=ifelse(bank$duration >=600,1,0)

bank$duration=NULL
bank$duration_code1=NULL


# a$duration_code1=ifelse(a$Freq<0.25,1,0)
# a$duration_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$duration_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$duration_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="duration"
# bank=merge(a,bank,by="duration")
# bank$duration=NULL
# bank$duration_code1=NULL
# str(bank)



a=data.frame(prop.table(table((bank$pdays),bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank$pdays_code1=ifelse(bank$pdays < 100,1,0)
bank$pdays_code2=ifelse(bank$pdays >= 100 & bank$pdays < 370,1,0)
bank$pdays_code3=ifelse(bank$pdays >= 370 ,1,0)

bank$pdays=NULL
bank$pdays_code2=NULL
# a$pdays_code1=ifelse(a$Freq<0.25,1,0)
# a$pdays_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$pdays_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$pdays_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="pdays"
# bank=merge(a,bank,by="pdays")
# bank$pdays=NULL
# bank$pdays_code1=NULL
# str(bank)



# a=data.frame(prop.table(table((bank$campaign),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# a$campaign_code1=ifelse(a$Freq<0.12,1,0)
# a$campaign_code2=ifelse(a$Freq>=0.12 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="campaign"
# bank=merge(a,bank,by="campaign")
# bank$campaign=NULL
# bank$campaign_code1=NULL
# str(bank)
# 
# 
# 
# bank$y= ifelse(bank$y=="yes",1,0 )
# bank$default =ifelse(bank$default=="yes",1,0)
# bank$housing =ifelse(bank$housing=="yes",1,0)
# bank$loan =ifelse(bank$loan=="yes",1,0)

str(bank)

bank=bank %>% select(-age,-job,-marital,-education,-default,-housing,-loan,-day,-previous,-campaign)

#==================Test========================


a=data.frame(prop.table(table((bank_test$balance),bank_test$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
dim(a)
ggplotly(p)


bank_test =bank_test %>%  mutate ( balance_code1 = ifelse(bank_test$balance<0,1,0),
                                   balance_code2 = ifelse(bank_test$balance>=0 & bank_test$balance<750,1,0),
                                   balance_code3 = ifelse(bank_test$balance>=750,1,0)
) %>% select(-balance)

# a
# a$balance_code1=ifelse(a$Freq<0.25,1,0)
# a$balance_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$balance_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$balance_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="balance"
# bank_test=merge(a,bank_test,by="balance")
# bank_test$balance=NULL
# bank_test$balance_code1=NULL


# a=data.frame(prop.table(table((bank_test$month),bank_test$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# a
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# a$code=ifelse(a$Freq>0.2,1,0)
# str(a)
# a=a %>% select(Var1,code)
# names(a)[1]="month"
# names(a)[2]="month_code"
# bank_test=merge(a,bank_test,by="month")
# bank_test$month=NULL
# 
# str(bank_test)
# table(bank_test$month_code)


a=data.frame(prop.table(table((bank_test$duration),bank_test$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank_test$duration_code1=ifelse(bank_test$duration <250,1,0)
bank_test$duration_code2=ifelse(bank_test$duration >=250 & bank_test$duration < 500,1,0)
bank_test$duration_code3=ifelse(bank_test$duration >=500 & bank_test$duration < 600,1,0)
bank_test$duration_code4=ifelse(bank_test$duration >=600,1,0)

bank_test$duration=NULL

# a$duration_code1=ifelse(a$Freq<0.25,1,0)
# a$duration_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$duration_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$duration_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="duration"
# bank_test=merge(a,bank_test,by="duration")
# bank_test$duration=NULL
# bank_test$duration_code1=NULL
# str(bank_test)



a=data.frame(prop.table(table((bank_test$pdays),bank_test$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank_test$pdays_code1=ifelse(bank_test$pdays < 100,1,0)
bank_test$pdays_code2=ifelse(bank_test$pdays >= 100 & bank_test$pdays < 370,1,0)
bank_test$pdays_code3=ifelse(bank_test$pdays >= 370 ,1,0)

bank_test$pdays=NULL

# a$pdays_code1=ifelse(a$Freq<0.25,1,0)
# a$pdays_code2=ifelse(a$Freq>=0.25 & a$Freq<0.5 ,1,0)
# a$pdays_code3=ifelse(a$Freq>=0.5 & a$Freq<0.75 ,1,0)
# a$pdays_code4=ifelse(a$Freq>=0.75 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="pdays"
# bank_test=merge(a,bank_test,by="pdays")
# bank_test$pdays=NULL
# bank_test$pdays_code1=NULL
# str(bank_test)



# a=data.frame(prop.table(table((bank_test$campaign),bank_test$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# a$campaign_code1=ifelse(a$Freq<0.12,1,0)
# a$campaign_code2=ifelse(a$Freq>=0.12 ,1,0)
# str(a)
# a=a %>% select(-Var2,-Freq)
# names(a)[1]="campaign"
# bank_test=merge(a,bank_test,by="campaign")
# bank_test$campaign=NULL
# bank_test$campaign_code1=NULL
# str(bank_test)
# 
# 
# 
# bank_test$y= ifelse(bank_test$y=="yes",1,0 )
# bank_test$default =ifelse(bank_test$default=="yes",1,0)
# bank_test$housing =ifelse(bank_test$housing=="yes",1,0)
# bank_test$loan =ifelse(bank_test$loan=="yes",1,0)

str(bank_test)

bank_test=bank_test %>% select(-age,-job,-marital,-education,-default,-housing,-loan,-day,-previous,-campaign)

##====== Character to factore ================
is.factor(bank[,2])
for(i in 1 : ncol(bank)){
  if(class(bank[,i])=="character"){
    bank[,i]=as.factor(bank[,i])
    
  }else{}
  
}


# ##====== stadadized int variable ================


##====== rf model on test and train ================
str(bank)


bank$y=as.factor(bank$y)
bank$month=as.factor(bank$month)
set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.80)


bank_train =bank[a,]
bank_test1 =bank[-a,]
x.bank_test=select(bank_test,-y)
y.bank_test=select(bank_test,y)
str(bank_train)
str(y.bank_test)
set.seed(2)

set.seed(1)
fitrf=randomForest(y~. ,
                   mtry=8,data=bank_train,ntree=500)
fitrf

fitrf$mtry


set.seed(1)
fitrf=randomForest(y~.-age_code4-default-age_code1-job_code3-marital_divorced-loan-age_code3-education_tertiary-marital_single-job_code2-education_secondary-age_code2-previous-housing-month_code-campaign-pdays,
                   mtry=3 ,
                   xtest=x.bank_test[, -which(names(x.bank_test) %in% c("pdays","campaign","month_code","housing","previous","age_code2","education_secondary","job_code2","marital_single","education_tertiary","age_code3","loan","marital_divorced","age_code4","default","age_code1","job_code3"))],ytest = as.factor(bank_test$y), 
                   data=bank_train,ntree=100)
fitrf


set.seed(1)
fitrf=randomForest(y~.-age_code4-default-age_code1-job_code3-marital_divorced-loan-age_code3-education_tertiary-marital_single-job_code2-education_secondary-age_code2-previous-housing,
                   mtry=3 ,
                   
                   data=bank_train,ntree=100)
fitrf

set.seed(1)
fitrf=randomForest(y~+duration+balance+day+pdays+campaign+month_code+housing,mtry=3 ,
                   data=bank_train,ntree=100)
fitrf


#####Final####

set.seed(1)
fitrf= randomForest(formula = y ~ . - age_code4 - default - age_code1 -      job_code3 - marital_divorced - loan - age_code3 - education_tertiary -      marital_single - job_code2 - education_secondary - age_code2 -      previous - housing, data = bank_train, 
                    mtry = 3, 
                    xtest = x.bank_test[,      -which(names(x.bank_test) %in% c("housing", "previous", "age_code2",          "education_secondary", "job_code2", "marital_single",          "education_tertiary", "age_code3", "loan", "marital_divorced",          "age_code4", "default", "age_code1", "job_code3"))],      ytest = as.factor(bank_test$y), 
                    ntree = 100) 
fitrf
##############

bank_cor=bank %>% select(y,duration,balance,day,pdays,campaign,month_code)
bank_cor=bank

bank_cor$y=as.numeric(bank_cor$y)
corrplot::corrplot(cor(bank_cor)) 

str((bank_cor))



# fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-score-yp,data=bank_train, ntree=200)
# fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-score-yp,data=bank_train, ntree=500) #90.40698
# fitrf=randomForest(y~duration+month+ age,data=bank_train, ntree= 200)
# fitrf=randomForest(y~.-default-contact-loan-marital-education-housing-previous-campaign-pdays-job -score-yp,data=bank_train, ntree=200)
summary(fitrf)

fitrf$importance 
varImpPlot(fitrf)
plot(fitrf)

score =predict(fitrf,bank_train)
table(bank_train$y,bank_train$score)
hist(bank_train$score)
score =predict(fitrf,bank_test1)
a=table(bank_test1$y,score)
hist(bank_test$score)


a=table(bank_train$y,as.numeric((bank_train$score)>0.1))
a
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
KS

a=table(bank_test$y,as.numeric((bank_test$score)>0.1))
a
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
KS

(a[2,2]+a[1,1])/ sum(a)

score =predict(fitrf,bank_test)
a=table(bank_test$y,score)
hist(bank_test$score)


a=table(bank_train$y,as.numeric((bank_train$score)>0.1))
a
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
KS

a=table(bank_test$y,as.numeric((bank_test$score)>0.1))
a
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
KS

(a[2,2]+a[1,1])/ sum(a)

#===================Test================================================

bank_test=read.csv("bank-full_test.csv",stringsAsFactors = F,na.strings = "unknown")
str(bank_test)

##====== Data modification=============

bank_test$job=gsub("[[:punct:]]" , "", bank_test$job)



##====== NA removal ================


bank_test[is.na(bank_test$job),"job"] = "NAJob"                             # Creating a new job category for NA


# Removing NA in education based on job

bank_test[ is.na(bank_test$education) & bank_test$job%in%c("bluecollar","admin","management","retired"),"education"] ="tertiary"
bank_test[ is.na(bank_test$education) & bank_test$job%in%c("technician","selfemployed","services"),"education"] ="secondary"
bank_test[ is.na(bank_test$education) ,"education"] ="primary"

# Removing NA by replacing with mode

bank_test[ is.na(bank_test$contact) ,"contact"] = "cellular"


# Removing poutcome as it contains many NAs
bank_test$poutcome =NULL
bank_test$ID =NULL


##====== Character to factore ================
is.factor(bank_test[,2])
for(i in 1 : ncol(bank_test)){
  if(class(bank_test[,i])=="character"){
    bank_test[,i]=as.factor(bank_test[,i])
    
  }else{}
  
}

bank_test$score =predict(fitrf,bank_test)
bank_test$score=ifelse(bank_test$score=="yes",1,0)
table(bank_test$score)

write.csv(bank_test$score,"3rf.csv",row.names = F)
