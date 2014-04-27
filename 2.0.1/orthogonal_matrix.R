library(pracma)

d=4
bla  <- matrix(data=NA,nrow = d, ncol=d)
for (i in 1:d){
  for (j in 1:d){    
  bla[i,j] <- rnorm(1,0,1)
  }
}

bla <- gramSchmidt(bla, tol = .Machine$double.eps^0.5)
blabla <- matrix(data=NA,nrow = d, ncol=d)
blabla  <- as.matrix(unlist(bla$Q))

blabla %*% t(blabla)