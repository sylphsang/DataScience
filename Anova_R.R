mydata=read.csv("newfood-paul.csv",header=TRUE)
attach(mydata)
names(mydata)
mydata
sales=Sales1+Sales2+Sales3
sales
price=factor(PriceLevel,levels=c(1,2,3),labels = c("LowPrice","MediumPrice","HighPrice"))
Advertisement=factor(AdLevel,levels=c(1,2),labels=c("lowAd","highAd"))
t.test(sales~Advertisement,var.equal=TRUE)
model1=aov(sales~price)
summary(model1)
model2=aov(sales~Advertisement)
summary(model2)
model3=aov(sales~price+Advertisement)
summary(model3)
model4=aov(sales~price+Advertisement+price*Advertisement)
summary(model4)
colors=c(rep("Brown",1),rep("Red",1))
interaction.plot(price,Advertisement,sales,col=colors,legend=TRUE)
model5=lm(sales~price+Advertisement+price*Advertisement)
result1=summary(model5)
model6=lm(sales~price+Advertisement)
result2=summary(model6)
result2
write.csv(result2$coefficients,file="c:/result2.csv")
ANCOVA=lm(sales~StoreSize+price+Advertisement+price*Advertisement)
summary(ANCOVA)
anova(ANCOVA)
library(car)
Anova



 