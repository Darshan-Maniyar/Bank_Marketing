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

# Removing NA by replacing with mode

# bank[ is.na(bank$contact) ,"contact"] = "cellular"


# Removing poutcome as it contains many NAs
# bank$poutcome =NULL
# bank$ID =NULL


# hist(log(bank$age))
# hist((bank$balance))
# hist((bank$balance+1)^-1)
# hist((bank$day))
# hist(log(bank$duration+1))
# hist((bank$campaign))
# hist((bank$pdays)^-1)
# hist((bank$pdays)^-1)

prop.table(table(bank$age,bank$y),1)
a=data.frame(prop.table(table(bank$age,bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
a
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)
# bank$age_code1 = 0
# bank$age_code2 = 0
# bank$age_code3 = 0
bank = bank %>%  mutate ( age_code1 = ifelse(bank$age<31,1,0),
                         age_code2 = ifelse(bank$age>=31 & bank$age < 60,1,0),
                         age_code3 = ifelse(bank$age>=60 & bank$age < 87,1,0),
                         age_code4 = ifelse(bank$age>=87,1,0)
                         ) %>% select(-age,age_code4)


a=data.frame(prop.table(table(bank$job,bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
a
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank = bank %>%  mutate ( job_code1 = ifelse(bank$job %in% c("bluecoller","housemaid","services","entrepreneur"),1,0),
                          job_code2 = ifelse(bank$job %in% c("technician","NAJob","selfemployed","admin","management","unemployed"),1,0),
                          job_code3 = ifelse(bank$job %in% c("retired","student"),1,0)
                          ) %>% select(-job,-job_code1)



a=data.frame(prop.table(table(bank$marital,bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
a

p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank = bank %>%  mutate ( marital_married = ifelse(bank$marital == "married",1,0),
                          marital_divorced = ifelse(bank$marital == "divorced",1,0),
                          marital_single = ifelse(bank$marital == "single",1,0)
                          ) %>% select(-marital,-marital_married)


a=data.frame(prop.table(table(bank$education,bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
a
table(bank$education)
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

bank = bank %>%  mutate ( education_primary = ifelse(bank$education == "primary",1,0),
                          education_secondary = ifelse(bank$education == "secondary",1,0),
                          education_tertiary = ifelse(bank$education == "tertiary",1,0)
) %>% select(-education,-education_primary)


# a=data.frame(prop.table(table(bank$default,bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)


# a=data.frame(prop.table(table((bank$balance),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# dim(a)
# ggplotly(p)


# bank =bank %>%  mutate ( balance_code1 = ifelse(bank$balance<0,1,0),
#                          balance_code2 = ifelse(bank$balance>=0 & bank$balance<750,1,0),
#                          balance_code3 = ifelse(bank$balance>=750,1,0)
#                          ) %>% select(-balance,-balance_code2)




# 
# 
# a=data.frame(prop.table(table((bank$contact),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# a
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# bank =bank %>%  mutate ( contact_cellular = ifelse(bank$contact=="cellular",1,0)
#                          ) %>% select(-contact)


# a=data.frame(prop.table(table((bank$day),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# a
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# a$code=ifelse(a$Freq>0.12,1,0)
# str(a)
# a=a %>% select(Var1,code)
# names(a)[1]="day"
# names(a)[2]="day_code"
# bank=merge(a,bank,by="day")
# bank$day=NULL

a=data.frame(prop.table(table((bank$month),bank$y),1))
a=a[a$Var2=="yes",]
a=a[order(a$Freq),]
a
p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
ggplotly(p)

a$code=ifelse(a$Freq>0.2,1,0)
str(a)
a=a %>% select(Var1,code)
names(a)[1]="month"
names(a)[2]="month_code"
bank=merge(a,bank,by="month")
bank$month=NULL

str(bank)
table(bank$month_code)


# a=data.frame(prop.table(table((bank$duration),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# bank$duration_code=ifelse(bank$duration >=500,1,0)
# bank$duration=NULL


# a=data.frame(prop.table(table((bank$pdays),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# 
# bank$pdays_code1=ifelse(bank$pdays < 0,1,0)
# bank$pdays_code2=ifelse(bank$pdays >= 0 & bank$pdays < 100,1,0)
# bank$pdays_code2=ifelse(bank$pdays >= 100 & bank$pdays < 375,1,0)
# bank$pdays_code2=ifelse(bank$pdays >= 375,1,0)
# bank$pdays=NULL


# a=data.frame(prop.table(table((bank$campaign),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# bank$campaign_code=ifelse(bank$campaign < 18,1,0 )
# bank$campaign=NULL


# a=data.frame(prop.table(table((bank$previous),bank$y),1))
# a=a[a$Var2=="yes",]
# a=a[order(a$Freq),]
# p=ggplot(a,aes(Var1,Freq))+geom_point()+geom_smooth()
# ggplotly(p)
# 
# bank$previous_code1 = ifelse(bank$previous< 12, 1,0 )
# bank$previous_code2 = ifelse(bank$previous>= 12 & bank$previous< 17, 1,0 )
# bank$previous_code3 = ifelse(bank$previous>= 17 & bank$previous< 23, 1,0 )
# bank$previous_code4 = ifelse(bank$previous>= 23 , 1,0 )
# bank$previous=NULL


bank$y= ifelse(bank$y=="yes",1,0 )
bank$default =ifelse(bank$default=="yes",1,0)
bank$housing =ifelse(bank$housing=="yes",1,0)
bank$loan =ifelse(bank$loan=="yes",1,0)

str(bank)

a[2000:4000,]
hist(bank$balance)

str(a)


a
bank$age1=ifelse(bank$age>10 & bank$age< )

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
set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.75)


bank_train =bank[a,]
bank_test =bank[-a,]
x.bank_test=select(bank_test,-y)
y.bank_test=select(bank_test,y)
str(bank_train)
str(y.bank_test)
set.seed(2)

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
