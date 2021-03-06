# Skirta aproksimuoti vienma�io normaliojo standartinio tikimybes
# siuo atveju kaskart perrinka nauja a.d. rinkini
# input: iteracij� skai�ius ir reik�m�
# output: tikimyb� 
univariateNormalStandard <- function(n,t){
  X <- rnorm(n)
  return(mean(X<t))
}


# Skirta aproksimuoti vienma�io normaliojo standartinio tikimybes
# nuo pirmo skirias, jog siuo atveju duodam viena rinkini a.d. ir pagal ji aproksimuoja
# input: sugeneruota a.d. realizacijos ir reik�m�
# output: tikimyb�
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
  #sum(apply(X<t,1,all))/n
  return(sum(apply(X<unlist(t),1,all))/n)
}

<<<<<<< HEAD:functions.R
# su viena fiksuota t reik�me didinam n (iteracij�) kiek�
# ir �i�rim kaip kei�iasi aproksimuota jos tikimyb�
# n -> inf, t -> tikra tikimyb�s reik�m� 
=======
# Suskaiciuoja dispersija pagal second approach ivercius
# kiekvienai reiksmei suskaiciuoja jos dispersija
# neaisku ar reikia kiekvienai, bet tarkim
# input: kiek a.d. generuot, X - a.d. realizacijos, t - tikroji reiksme, ets- ivertis
# output: grazina dispersija kiekvienai reiksmei
varUniNormStand <- function(n,X,t,est) {
  return((sum(((X<t)*1-est)^2))/(n^2))
}

# su viena fiksuota t reik�me didinam n (iteracij�) kiek�
# ir �i�rim kaip kei�iasi aproksimuota jos tikimyb�
# n -> inf, est -> tikra tikimyb�s reik�m� 
>>>>>>> 6ece7bb0e38a3ebd189729070a41fd7c5e82880a:functionsNormal.R
#input: n - iteracij� skai�ius (skai�iuojama su visais skirtingais skai�iais 1:n), t - reiksme
#output: aproksimuot� tikimybi� su skirtingais n vektorius

risingNunivariateNormalStandard  <- function(n,t){
  m <- c()
  for (i in 1:n){
    m[i] <- univariateNormalStandard(i,t)  
  }
  return(m)
}