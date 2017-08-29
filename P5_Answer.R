library(ggplot2)
getwd()
a=read.csv("bank-full_train.csv",stringsAsFactors = F)
View(a)
summary(a)
str(a)

# 1. Find mean of the variable age. Round off to 2 decimal places.
round(mean(a$age),2)

# 2. Total number of outliers present in the variable balance.Use ‘Q1-1.5*IQR’ to calculate lower limit and ‘Q3 + 1.5×IQR’ to calculate upper limit. 
# calculate the count of values in variable balance which are beyond these limits.
a$balance
ggplot(a,aes(x=balance))+geom_histogram()
ggplot(a,aes(y=balance,x=1))+geom_boxplot()
mean(a$balance)
median(a$balance)

b1=quantile(a$balance)[2]
b2=quantile(a$balance)[4]
c=IQR(a$balance)

min(a$balance)
lwlim=b1-(1.5*(c))
lwlim
hilim=b2+(1.5*(c))
hilim
max(a$balance)

OL=c(a$balance[a$balance<lwlim],a$balance[a$balance>hilim])
length(OL)
sum(table(OL))
max(good)
min(good)
good=a$balance[!(a$balance%in%OL)]
ggplot(,aes(x=good))+geom_histogram()
ggplot(,aes(x=good))+geom_line(stat = "density")
ggplot(,aes(y=good,x=1))+geom_boxplot()


# 3. Find the variance of variable balance
var(a$balance)
sum(((a$balance)-(round(mean(a$balance),2)))^2)/length(a$balance)

#4. which function is used to remove multicollinearity among variables?
#Note: Answers are not case sensitive 
library(car)
?vif

#5.  Model with 'lower AIC' value is a better model or the model with 'higher AIC' value?
# Note: Write either 'lower AIC' or 'higher AIC', any further details will result in your answer being marked as wrong. Answers are not case sensitive . 
      
lower AIC

#6. Should the variable ID be included in building the model?
# Note : Just write 'Yes' or 'No' . If you write sentences , automated grading will consider it incorrect .Answers are not case sensitive . 

No

#7. Does validation help in generalising the model?
# Note : Just write 'Yes' or 'No' . If you write sentences , automated grading will consider it incorrect .Answers are not case sensitive . 

Yes

#8. Whether the data given (bank-full_train) is a balanced or imbalanced data?
# Note: you need to write either 'Balanced' or 'imbalanced' . 
# Any further details will result in your answer being marked as wrong. Answers are not case sensitive . 
str(a)
table(a$y)
sum(is.na(a$y))

imbalanced

#9. How is box plot upper whisker is calculated ? Choose out of these:
Q3 + 1.5×IQR

#10. R2 or adjusted R2, which metric to be used to check goodness of the model?
# Note: Answers are not case sensitive . 

adjusted R2