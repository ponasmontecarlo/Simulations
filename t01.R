library(mvtnorm)
library(reshape2)
library(ggplot2)
library(xtable)
################
##### UNIVARIATE
################

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
  ggtitle("Vienmaèio pasiskirstymo f-ja su 2 SE")


### Table for univariate
### N vs t
N <- c(10,100,1000,10000,100000,1000000,10000000,100000000)
X <- rt(100000000,1)
t <- c(-3.2,-1,0,0.5,1,2,3.2)

res=matrix(0,ncol=7,nrow=9)
for (i in 2:9) {
   for (j in 1:7) {
    res[i-1,j]=mean(X[1:N[i-1]]<t[j])
   }
}
res[9,] <- pt(t,1)
final <- matrix(as.numeric(format(res,digits=3)),ncol=7)
final <- data.frame(final)
final <- cbind(c(N,"Tikroji"),final)
colnames(final) <- c("N",t)
rownames(final) <- NULL
xtable(final,digits=4)

#### N vs var
# !!! BEWARE OF HUGE LAG !!!!
# LAG LEVEL OVER 9000
# ~4-5gb of ram needed
resVar=matrix(0,ncol=7,nrow=9)
for (i in 2:9) {
  for (j in 1:7) {
    resVar[i-1,j]=tUnivariateVar(N[i-1],X[1:N[i-1]],t[j],res[i-1,j])
  }
}
finalVar <- matrix(as.numeric(format(resVar[1:8,],digits=3)),ncol=7)
finalVar <- data.frame(finalVar)
finalVar <- cbind(N,finalVar)
colnames(finalVar) <- c("N",t)
rownames(finalVar) <- NULL
xtable(finalVar,display=c(rep("e",9)))

#### N vs df
df <- c(1,2,5,10,25,50,120,Inf)
resDf=matrix(0,ncol=7,nrow=8)
X <- sapply(df,function(x) rt(100000,x))
for (i in 2:9) {
  for (j in 1:7) {
    resDf[i-1,j]=mean(X[,i-1]<t[j])
  }
}
finalDf <- matrix(as.numeric(format(resDf,digits=3)),ncol=7)
finalDf <- rbind(finalDf,sapply(t,function(x) mean(rnorm(100000)<x)))
finalDf <- data.frame(finalDf)
finalDf <- cbind(c(df,"normal.stand"),finalDf)
colnames(finalDf) <- c("DF",t)
rownames(finalDf) <- NULL
xtable(finalDf,digits=4)


###############
##### BIVARIATE
###############

n <- 1000 # realizacijø skaièius
df <- 1 # degrees of freedom
sigma <- diag(2) # kovariaciju matrica
mu <- rep(0,2) # vidurkiu vektorius
X <- rmt(n,mu,sigma,df) # a.d. realizacijos
t <- data.frame(t1=seq(-3.2,3.2,0.01),t2=seq(-3.2,3.2,0.01)) # reiksmes

estBi <- rep(NA,641)

for (i in 1:641) {
  estBi[i] <- tMultivariate(X,t[i,],n)
}

dataBi <- data.frame(t=t[,1],estimate=estBi,prob=apply(t,1,function(x) pmt(x,mu,sigma,df)))
dataBi <- melt(dataBi,id="t")
ggplot(dataBi,aes(t,value,colour=variable))+geom_line()+ggtitle("Bivariate t-distribution")


