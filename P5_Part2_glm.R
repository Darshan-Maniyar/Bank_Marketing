library(ggplot2)
library(dummy)
library(dplyr)
library(car)

getwd()
bank=read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = "unknown")
# View(bank)
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
# bank=bank[,-16]
bank$poutcome[is.na(bank$poutcom)]="NA"

##====== Dummy variable creation ================
str(bank)
summary(bank)
apply(bank,2,unique)
round(prop.table(table(bank$job,bank$y),1),2)

bank= bank %>% mutate(job = ifelse(bank$job==c("student","retired"),"1",ifelse(bank$job==c("unemployed","management","admin","NAJob","selfemployed","technician"),"2","3")))


round(prop.table(table(bank$month,bank$y),1),2)

bank= bank %>% mutate(month = ifelse(bank$month==c("mar","oct","dec","sep"),"1",ifelse(bank$month==c("apr","feb","aug","jan","nov"),"2","3")))

bank=dummy::dummy(bank,c("job","marital","education","default","housing","loan","contact","month"))

# bank = bank[,-8]
bank$y=ifelse(bank$y=="yes",1,0)
bank$poutcome=NULL
bank$ID=NULL
##====== glm model ================
str(bank)

#glm on complete train data
fitglm=glm(y_no~.,data=bank)
summary(fitglm)
sort(vif(fitglm),decreasing = T)


fitglm=glm(y~.-day-contact_cellular,data=bank,family="binomial")
summary(fitglm)
pre=predict(fitglm,bank,type = "response")
hist(pre)
table(bank$y[bank$y==1])/table(bank$y[bank$y==0])
pre=ifelse(pre<0.13,0,1)
a=table(bank$y,pre)
TPR=a[2,2]/ sum(a[2,])
FPR=a[1,2]/ sum(a[1,])
KS=TPR-FPR
##====== glm model on test and train ================
str(bank)
set.seed(2)
a=sample(1:nrow(bank),nrow(bank)*0.75)

bank_train =bank[a,]
bank_test =bank[-a,]

fitglm=glm(y_no~.,data=bank_train)
summary(fitglm)
sort(vif(fitglm),decreasing = T)


fitglm=glm(y_no~.-day-contact_cellular-default_no,data=bank_train)
summary(fitglm)

bank_train$score =predict(fitglm,bank_train)
bank_test$score =predict(fitglm,bank_test)

a=confmatrix(bank_train$y_no,bank_train$score,"KS",bank_test$y_no,bank_test$score) #0.848

0.7745831/0.2254169


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


##====== Dummy variable creation ================
str(bank_t)
summary(bank_t)
apply(bank_t,2,unique)
round(prop.table(table(bank_t$job,bank_t$y),1),2)

bank_t= bank_t %>% mutate(job = ifelse(bank_t$job==c("student","retired"),"1",ifelse(bank_t$job==c("unemployed","management","admin","NAJob","selfemployed","technician"),"2","3")))


round(prop.table(table(bank_t$month,bank_t$y),1),2)

bank_t= bank_t %>% mutate(month = ifelse(bank_t$month==c("mar","oct","dec","sep"),"1",ifelse(bank_t$month==c("apr","feb","aug","jan","nov"),"2","3")))

bank_t=dummy::dummy(bank_t,c("job","marital","education","default","housing","loan","contact","month"))

bank_t = bank_t[,-8]

bank_t$score =predict(fitglm,bank_t)
bank_t$y= ifelse(bank_t$score>0.848,"yes","no")
table(bank_t$y)

# cutoff=0.848
write.csv(bank_t$y, file = "Submission11.csv",row.names = F)
# 
# no  yes 
# 3977 9587
