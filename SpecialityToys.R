#given mean
mean=20000
# given p(10k<x<30k)=0.95 
#as mean is 20000 by symmetry rule 
# of normal table, z value is 1.96 standard deviation is 10000/1.96
sd=as.integer(10000/1.96)
sd


lb=10000
ub=30000

x<-seq(-3.5,3.5,.01)*sd+mean
x
hx=dnorm(x,mean,sd)
hx
plot(x,hx,type='n',xlab="Expected Demand",ylab="",main="Normal Distributions",axes=FALSE)

i <- x>=lb & x<=ub
lines(x,hx)
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="sky blue")
area<-pnorm(ub,mean,sd)-pnorm(lb,mean,sd)
result <- paste("P(",lb,"<Demand<",ub,")=",signif(area,digits = 3))

mtext(result,3)
axis_bound=5
axis(side=1,at=seq(-axis_bound*sd+mean,axis_bound*sd+mean,by=sd),pos=0)

#calculating stock out probabilities
pnorm(15000,mean,sd,lower.tail = FALSE)

pnorm(18000,mean,sd,lower.tail = FALSE)

pnorm(24000,mean,sd,lower.tail = FALSE)

pnorm(28000,mean,sd,lower.tail = FALSE)


############PROFIT/LOSS###########################3
profit_init=24-16
profit_init

profit_later=16-5

################  for 15000 ####################
#worst case scenerio 10000

projected_profit=8*10000-11*5000
projected_profit

#for most likely scenerio 20000

projected_profit=8*15000
projected_profit

#for best case scenerio

projected_profit=8*15000
projected_profit


################  for 18000 ####################
#worst case scenerio 10000

projected_profit=8*10000-11*8000
projected_profit

#for most likely scenerio 20000

projected_profit=8*18000
projected_profit

#for best case scenerio

projected_profit=8*18000
projected_profit



################  for 24000 ####################
#worst case scenerio 10000

projected_profit=8*10000-11*14000
projected_profit

#for most likely scenerio 20000

projected_profit=8*20000-11*4000
projected_profit

#for best case scenerio

projected_profit=8*24000
projected_profit
 