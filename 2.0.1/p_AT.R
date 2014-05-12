library(Matrix)
library(mnormt)

#$\hat{p}_{AT}$
#M - Monte Carlo simuliacijø skaièius
#mu - vidurkiø vektorius
#sigma - kovariacijø matrica
#t - ieðkomø tikimybiø vektorius

p_AT <- function(M,mu,sigma,t){
  n  <- length(t)
  I  <- diag(rep(1,n))
  L  <- t(chol(sigma))
  
  
  X_plus  <- matrix(data=NA,nrow=M,ncol=n)
  X_minus  <- matrix(data=NA,nrow=M,ncol=n)
  z <- rmnorm(n=M,mean=rep(0,n),varcov=I)
  
  for(i in 1:M){
    X_plus[i,]  <- rbind(mu + as.matrix(L)%*%cbind(as.vector(z[i,])))
    X_minus[i,] <- rbind(mu - as.matrix(L)%*%cbind(as.vector(z[i,])))
  }
  
  plus  <- sum(apply(X_plus<unlist(t),1,all))
  minus <- sum(apply(X_minus<unlist(t),1,all))
  return((plus + minus)/(2*M))
}


# test
M  <- 10000
mu  <-c(0,0,0)
sigma  <- diag(1,3)
t <- c(0.5,0.5,0.5)

result  <- p_AT(M,mu,sigma,t)
pmnorm(t,mu,sigma)

result

