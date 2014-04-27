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
  chi <- c()
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
ttt <- c(0.5,0.5,0.5)

L  <- t(chol(sigma))
unit <- unitV(n/2,d)
x_sum_plus <- c()
x_sum_minus <- c()

for (i in 1:M){
  ortho <- orthoT(d)
  r <- radius(k,n/2)
  z_plus <- matrix(data=NA,nrow=n/2,ncol=d)
  z_minus <- matrix(data=NA,nrow=n/2,ncol=d)
  x_plus <- matrix(data=NA,nrow=n/2,ncol=d)
  x_minus <- matrix(data=NA,nrow=n/2,ncol=d)
  for (j in 1:(n/2)){
    z_plus[j,] <- r[j]*as.matrix(ortho)%*%cbind(as.vector(unit[j,]))
    z_minus[j,] <- -z_plus[j,]
    
    x_plus[j,]  <- rbind(mu + as.matrix(L)%*%cbind(as.vector(z_plus[j,])))
    x_minus[j,] <- rbind(mu + as.matrix(L)%*%cbind(as.vector(z_minus[j,])))
  }
  x_sum_plus[i]  <- sum(apply(x_plus<unlist(ttt),1,all))
  x_sum_minus[i]  <- sum(apply(x_minus<unlist(ttt),1,all))
}

sum <- (sum(x_sum_plus)+sum(x_sum_minus))/(2*M*n/2)
sum