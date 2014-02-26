# Skirta aproksimuoti vienmaèio normaliojo standartinio tikimybes
# siuo atveju kaskart perrinka nauja a.d. rinkini
# input: iteracijø skaièius ir reikðmë
# output: tikimybë
univariateNormalStandard <- function(n,t){
  X <- rnorm(n)
  return(mean(X<t))
}


# Skirta aproksimuoti vienmaèio normaliojo standartinio tikimybes
# nuo pirmo skirias, jog siuo atveju duodam viena rinkini a.d. ir pagal ji aproksimuoja
# input: sugeneruota a.d. aibe ir reikðmë
# output: tikimybë
univariateNormalStandard2 <- function(X,t){
  return(mean(X<t))
}

# aproksimuoja n-macio standartinio normalio tikimybes
# erdve priklauso nuo paduoto ieskomu reiksmiu vektoriaus ilgio
# siuo atveju irgi kiekvienai reiksmei perrenka nauja a.d. rinkini
# input: iteraciju skaicius ir vektorius reiksmiu, kuriu tikimybes ieskom
# output: tikimybe
multivariateNormalStandard <- function(n,t){
  m <- length(t)
  mu <- rep(0,m)
  varcov <- diag(rep(1,m))
  X <- rmnorm(n,mu,varcov)
  sum(apply(X<t,1,all))/n
  return(sum(apply(X<unlist(t),1,all))/n)
}