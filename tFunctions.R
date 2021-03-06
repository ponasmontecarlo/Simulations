###############
#### UNIVARIATE
###############

# tas pats, kas normaliuoju atveju. tik paduodam realizacijas su parinktu 
# degrees of freedom skai�iumi
# input: a.d. realizacijos, reik�m�
# output: aproksimuota tikimyb�

tUnivariate <- function(X,t) {
  mean(X<t)
}

# skaiciuoja dispersija pagal apibreziam
# input: realizacij� skai�ius, a.d. realizacijos, tikrosios reik�m�s, �ver�iai
# output: dispersijos reik�m�s

tUnivariateVar <- function(n,X,t,est) {
  return((sum(((X<t)*1-est)^2))/(n^2))
}

#################
#### MULTIVARIATE
#################

# suskaiciuoja daugiamciu atveju
# input: a.d. realizaciju matrica, reiksmiu vektorius ir realizaciju sk
# output: tikimybes ivertis

tMultivariate <- function(X,t,n){
  return(sum(apply(X<unlist(t),1,all))/n)
}