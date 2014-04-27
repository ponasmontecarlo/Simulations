library(pracma)
library(mnormt)


##############################

orthoT  <- function(d){
matrica  <- matrix(data=NA,nrow = d, ncol=d)
  for (i in 1:d){
    for (j in 1:d){    
    matrica[i,j] <- rnorm(1,0,1)
    }
  }
matrica <- gramSchmidt(matrica, tol = .Machine$double.eps^0.5)
matricaT <- matrix(data=NA,nrow = d, ncol=d)
matricaT  <- as.matrix(unlist(matrica$Q))
return(matricaT)
}

###############################


k=3
sum=0

# k - laisves laipsniai
# j - radius skaicius(kiekis)
radius <- function(k,j){
 c hi <- c()
  for (i in 1:j){
    sum = 0
    t <- rnorm(k,0,1)
    for (h in 1:k){
      sum  =  sum + t[h]^2
    }
    chi[i] <- sqrt(sum)
    
  }
return(chi)
}
# k - vienetiniu vektoriu skaicius(kiekis)
# d - dimensiju skaicius
unitV <- function(k,d){
  t <- matrix(data=NA,nrow=k,ncol=d)
  for (i in 1:k){
    sum  <- 0
    t[i,] <- rnorm(n=d,0,1)#,mean=rep(0,d),varcov=diag(rep(1,d)))
    for (h in 1:d){
      sum <- sum + t[i,h]^2
    }
    t[i,] <-  t[i,]/sqrt(sum)
  }
  return (t)
}
#-----------------------
L  <- t(chol(sigma))

for(i in 1:M){
  X[i,]  <- rbind(mu + as.matrix(L)%*%cbind(as.vector(z[i,])))
}


d=3
k=d
M=1000
n=100
sigma  <- diag(1,d)
mu <- rep(0,d)
t <- c(0.5,0.5,0.5)

L  <- t(chol(sigma))
unit <- unitV(n,d)
x_sum <- c(data=NA)

for (i in 1:M){
  ortho <- orthoT(d)
  r <- radius(k,n)
  z <- matrix(data=NA,nrow=n,ncol=d)
  x <- matrix(data=NA,nrow=n,ncol=d)
  for (j in 1:n){
    z[j,] <- r[j]*as.matrix(ortho)%*%cbind(as.vector(unit[j,]))
    x[j,]  <- rbind(mu + as.matrix(L)%*%cbind(as.vector(z[j,])))
  }
  x_sum[i]  <- sum(apply(x<unlist(t),1,all))
}

sum <- sum(x_sum)/(M*n)
sum
