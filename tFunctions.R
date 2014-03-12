###############
#### UNIVARIATE
###############

# tas pats, kas normaliuoju atveju. tik paduodam realizacijas su parinktu 
# degrees of freedom skaièiumi
# input: a.d. realizacijos, reikðmë
# output: aproksimuota tikimybë

tUnivariate <- function(X,t) {
  mean(X<t)
}

# skaiciuoja dispersija pagal apibreziam
# input: realizacijø skaièius, a.d. realizacijos, tikrosios reikðmës, áverèiai
# output: dispersijos reikðmës

tUnivariateVar <- function(n,X,t,est) {
  return((sum(((X<t)*1-est)^2))/(n^2))
}