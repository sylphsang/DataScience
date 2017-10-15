d<-data.frame(y=as.factor(sample(0:1,100,repl=T)),age=rnorm(100,50,34))
str(d)

glm(y~age,data=d,family=binomial((link="logit"))) ->ft
ft
-2*sum(ifelse(d$y==1,log(.45),log(.55)))
predict(ft,d,type="resp")
#log(odds) =