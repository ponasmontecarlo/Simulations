library(ggplot2)
library(reshape2)
library(mnormt)
library(plyr)
source("functions.R")

#### UNIVARIATE ########

n <- 1000
est <- rep(NA,641)
t <- seq(-3.2,3.2,0.01)
for (i in 1:641) {
  est[i] <- univariateNormalStandard(n,t[i])
}

data <- data.frame(t=t,estimate=est,prob=pnorm(t))
data <- melt(data,id="t")
ggplot(data,aes(t,value,colour=variable))+geom_line()+ggtitle("Vienmatis atvejis")

##### UNIVARIATE 2nd APPROACH #####

n <- 1000
X <- rnorm(n)
est <- rep(NA,641)
t <- seq(-3.2,3.2,0.01)
for (i in 1:641) {
  est[i] <- univariateNormalStandard2(X,t[i])
}

data <- data.frame(t=t,estimate=est,prob=pnorm(t))
data <- melt(data,id="t")
ggplot(data,aes(t,value,colour=variable))+geom_line()+ggtitle("Vienmatis atvejis")


######## multivariate #######

# 2-dimensial
n <- 1000
estMul <- rep(NA,641)
t <- data.frame(t1=seq(-3.2,3.2,0.01),t2=seq(-3.2,3.2,0.01))
for (i in 1:641){
  estMul[i] <- multivariateNormalStandard(n,t[i,])
}

data <- data.frame(t=t[,1],estimate=estMul,prob=apply(t,1,function(x) pmnorm(x,rep(0,2),diag(rep(1,2)))))
data <- melt(data,id="t")
ggplot(data,aes(t,value,colour=variable))+geom_line()+ggtitle("Dvimatis atvejis")

# 3-dimensial
n <- 1000
estMul <- rep(NA,641)
t <- data.frame(t1=seq(-3.2,3.2,0.01),t2=seq(-3.2,3.2,0.01),t3=seq(-3.2,3.2,0.01))
for (i in 1:641){
  estMul[i] <- multivariateNormalStandard(n,t[i,])
}

data <- data.frame(t=t[,1],estimate=estMul,prob=apply(t,1,function(x) pmnorm(x,rep(0,3),diag(rep(1,3)))))
data <- melt(data,id="t")
ggplot(data,aes(t,value,colour=variable))+geom_line()+ggtitle("Trimatis atvejis")
