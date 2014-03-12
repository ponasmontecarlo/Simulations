library(mvtnorm)
library(reshape2)
library(ggplot2)
################
##### UNIVARIATE
################

##### simulations

n <- 1000 # realizacijø skaièius
df <- 1 # degrees of freedom
X <- rt(n,df) # a.d. realiacijos
t <- seq(-3.2,3.2,0.01) # reikðmës
p <- pt(t,df) # tikimybës

est <- rep(NA,641)
var <- rep(NA,641)

for (i in 1:641) {
  est[i] <- tUnivariate(X,t[i])
}

for (i in 1:641) {
  var[i] <- tUnivariateVar(n,X,t[i],est[i])
}

dataUni <- data.frame(t=t,estimate=est,prob=p,
                      low=-2*sqrt(var)+est,high=2*sqrt(var)+est)
dataUni <- melt(dataUni,id=c("t","low","high"))
ggplot(dataUni,aes(t,value,colour=variable))+geom_line()+
  geom_ribbon(aes(ymin=low,ymax=high),fill="yellow",colour=NA,alpha=0.5)+
  ggtitle("Vienmaèio pasiskirstymo f-ja")

