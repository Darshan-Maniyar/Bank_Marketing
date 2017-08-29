library(dumm)
library(xgboost)
library(dplyr)

bank=read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = "unknown")
str(bank)
colSums(is.na(bank))
bank$ID=NULL
apply(bank,2,function(x) { if (class(x)=="character") {unique(x)}else{}})

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

library(xgboost)
library(caret)
b=bank
str(b)
#Variable removla

# z=which(names(b) %in%c("duration"))
# b=b[-z]
b$y=ifelse(b$y=="yes",1,0)
# table(b$previous)


# b=dummy(b,c("education","default","housing","loan","contact","poutcome"),limit = 50,rm_original = T)
b=dummy(b,c("marital","job","education","default","housing","loan","contact","month","poutcome"),limit = 50,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
a=which(names(b) %in%"y")
b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)




# b1=as.matrix(b[a])
# b2=as.matrix(b[-a])
# 
# xgbd=xgb.DMatrix(data=b2,label=b1)


b1=as.matrix(b_train[a])
b2=as.matrix(b_train[-a])

b3=as.matrix(b_test[a])
b4=as.matrix(b_test[-a])


xgbd=xgb.DMatrix(data=b2,label=b1)
xgbdtest=xgb.DMatrix(data=b4,label=b3)



param <- list("eta" =0.1,
              
              "max.depth"=9,
              # "min_child_weight"=0.9,
              "gamma"=0,
              # "lambda"=10,"lambda_bias"=0,"alpha"=2,
              "nthread"=32,
              "subsample"=0.9,
              # "colsample_bytree"=0.5,
              # "scale_pos_weight"=1,
              "eval_metric" = confmatrix_KS,
              
              "objective"="binary:logistic"
              # "booster"="dart",
              # "sample_type"="weighted",
              # "normalize_type"="forest"
) 
set.seed(2)
bst.cv = xgb.cv(param=param,  xgbd ,nrounds=3000 ,early_stopping_rounds=50,nfold = 4,seed=27,"maximize"=T)#,niter=10,  ,niter=500, nrounds = 50,colsample_bytree =0.9,gamma =100,min_child_weight=10,subsample =0.9)#)#prediction=TRUE, verbose=FALSE,
bst.cv$niter
bst.cv$best_iteration
max.auc.idx = which.max(bst.cv$evaluation_log[,test_auc_mean]) 
max.auc.idx 
bst.cv$evaluation_log[max.auc.idx,]
summary(bst.cv)
plot(bst.cv$evaluation_log[,iter],bst.cv$evaluation_log[,test_KS_mean])


set.seed(2)
bst <- xgboost(param=param,xgbd ,nrounds=121 ,nfold = 4,seed=27,verbose = 0)#,niter=299, early_stopping_rounds=50,min_child_weight=10,subsample =0.9)#


names <- dimnames(b2)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:25])

btr=b_train

set.seed(2)
btr$pred=predict(bst,xgbd,ntreelimit = 121)
a2=confmatrix(btr$y,btr$pred,model = "KS")
View(a2$table)
a2=table(btr$y,as.numeric(btr$pred>0.253))
(a2[2,2]/sum(a2[2,]))- (a2[1,2]/sum(a2[1,]))
(nrow(a2)+ncol(a2))==4

##============== test on test set=============

bt=b_test
set.seed(2)
bt$pred=predict(bst,xgbdtest,ntreelimit = 121)#as.matrix(h1_test[2:425])

a2=confmatrix(bt$y,bt$pred,model = "KS")
unlist(a2$KS, use.names=FALSE)
(974/(974+142))-(1810/(1810+65569))


a2=table(bt$y,as.numeric(bt$pred>0.253))
(a2[2,2]/sum(a2[2,]))- (a2[1,2]/sum(a2[1,]))
ks <- function(preds, y) {
  label=getinfo(y,"label")
  k=(table(preds>0.2,label))
  if((nrow(k)+ncol(k))==4){
  k1=(k[2,2]/sum(k[2,]))- (k[1,2]/sum(k[1,]))}
  else{k1=0}
  return(list(metric = "KS", value = as.numeric(k1)))
}
# 
# kappa <- function(preds, y) {
#   label=getinfo(y,"label")
#   k=vcd::Kappa(table(preds>0.5,label))
#   return(list(metric = "kappa", value = as.numeric(k$Unweighted[1])))
# }


x=data.frame(m= 1:500)
preds=bt$pred

set.seed(2)
z=apply(x,1,function(y){
set.seed(y)  
a=sample(1:nrow(b_test),nrow(b_test)*0.01)
bt1=b_test[a,]
a=which(names(bt1) %in%"y")
b5=as.matrix(bt1[a])
b6=as.matrix(bt1[-a])
xgbdtest1=xgb.DMatrix(data=b6,label=b5)

bt1$pred=predict(bst,xgbdtest1,ntreelimit =73 )#as.matrix(h1_test[2:425])
a2=confmatrix(bt1$y,bt1$pred,model = "KS")

return(c(unlist(a2["KS"], use.names=FALSE),unlist(a2["cutoff"], use.names=FALSE)))#,a2["Matrix"]
})
l=data.frame(t(z))
View(l)
a3=a2$Matrix
(a3[2,2]/sum(a3[2,]))- (a3[1,2]/sum(a3[1,]))
summary(l)
#0.1310
hist(l$X2)


##===============Prediction==============

bank=read.csv("bank-full_test.csv",stringsAsFactors = F,na.strings = "unknown")
str(bank)
colSums(is.na(bank))
bank$ID=NULL
apply(bank,2,function(x) { if (class(x)=="character") {unique(x)}else{}})

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

library(xgboost)
library(caret)
b=bank
str(b)

b=dummy(b,c("marital","job","education","default","housing","loan","contact","month","poutcome"),limit = 50,rm_original = T)

b=scale(b)


b1=as.matrix(b)
xgbd=xgb.DMatrix(data=b1)

bank$pred=predict(bst,xgbd,ntreelimit = 121)#as.matrix(h1_test[2:425])

bank$y=as.numeric(bank$pred>0.131)


write.csv(bank$y, file = "3_xgb.csv",row.names = F)

##===============Split test and train==============

set.seed(2)
b=sample(1:nrow(h1),nrow(h1)*0.8)
h1_train=h1[b,]
h1_test=h1[-b,]

require(DiagrammeR)
str(h1_train)
dim(h1_test)
h5=as.matrix(h1_train[-a])
h2=as.matrix(h1_train[a])

h3=as.matrix(h1_test[-a])
h4=as.matrix(h1_test[a])

xgmat <- xgb.DMatrix(data=h5, label = h2)#,missing = -999
xgmat1 <- xgb.DMatrix(data=h3, label = h4)#,missing = -999
