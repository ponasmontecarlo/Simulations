library(mvtnorm)
library(reshape2)
library(ggplot2)
library(xtable)
library(mnormt)


source("tFunctions.R") # sauce veikia, kai ta pati wd direktorija

### Vienmatis atvejis, didinam N ir ziurim kas darosi ###
n <- 2000 # realizacijų skaičius
df <- 1 # degrees of freedom
t <- 0 # 'reiksme' kurioje tikrinam
m <- rep(NA,n)

for (i in 1:n){ 
X <- rt(i,df) # a.d. realizacijos
m[i]  <- tUnivariate(X,t)
}

p <- pt(t,df) # tikra reiksme
plot(m, type='l')

h <- seq(1,n,1)

dataUni <- data.frame(h=h, prob=rep(pt(t,df),n), estimate=m)
dataUni <- melt(dataUni,id="h")
ggplot(dataUni,aes(h,value,colour=variable))+geom_line()+ggtitle("pam")





### Dvimatis atvejis, didinam N ir ziurim kas darosi ###

n <- 2000 # realizacijų skaičius
df <- 1 # degrees of freedom
sigma <- diag(2) # kovariaciju matrica
mu <- rep(0,2) # vidurkiu vektorius
t <- list(0,0) # 'reiksme' kurioje tikrinam
m <- rep(NA,n)

for (i in 1:n) {
  X <- rmt(i,mu,sigma,df) # a.d. realizacijos
  m[i] <- tMultivariate(X,t,i)
}

plot(m, type='l');

h <- seq(1,n,1)

dataMulti <- data.frame(h=h, estimate=m) ### truksta tikros reiksmes line
dataMulti <- melt(dataMulti,id="h")
ggplot(dataMulti,aes(h,value,colour=variable))+geom_line()+ggtitle("pampam")
