# tikroji dispersija skaièiuojama taip (pagal knygos 68psl)

t <- 0
n <- 1000

(pnorm(t)*(1-pnorm(t)))/n

#### UNIVARIATE


# Áverèiø ir dispersijos grafikas |SU ANTRU APPROACHU!|
n <- 1000
X <- rnorm(n)
est <- rep(NA,641)
t <- seq(-3.2,3.2,0.01)
for (i in 1:641) {
  est[i] <- univariateNormalStandard2(X,t[i])
}

var <- rep(NA,641)
for (i in 1:641) {
  var[i] <- varUniNormStand(n,X,t[i],est[i])
}

data <- data.frame(t=t,estimate=est,prob=pnorm(t),se.low=est-2*sqrt(var),se.high=est+2*sqrt(var))
data <- melt(data,id="t")
ggplot(data,aes(t,value,colour=variable))+geom_line()+ggtitle("Vienmatis su dispersija atvejis")
