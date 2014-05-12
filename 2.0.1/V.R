#### 2D

a1 <- c(-.707107000000E+00,  .122474500000E+01)
a2 <- c( .707107000000E+00, -.122474500000E+01)
a3 <- c( .141421400000E+01,  .000000000000E+00)
a4 <- c(-.141421400000E+01,  .000000000000E+00)
a5 <- c( .707107000000E+00,  .122474500000E+01)
a6 <- c(-.707107000000E+00, -.122474500000E+01)

Vmatrix  <- rbind(a1,a2,a3,a4,a5,a6)

#normalization
for (i in 1:6){
  length  <- sqrt(Vmatrix[i,1]^2 + Vmatrix[i,2]^2)
  Vmatrix[i,] <-  Vmatrix[i,]/length
} 

plot(Vmatrix[,1],Vmatrix[,2])

write.table(Vmatrix,'vektoriai/A2.txt',row.names=F,col.names=F)

#### 3D

b1  <- c( .000000000000E+00, -.816497000000E+00,  .115470100000E+01)
b2  <- c( .000000000000E+00,  .816497000000E+00, -.115470100000E+01)
b3  <- c(-.707107000000E+00,  .122474500000E+01,  .000000000000E+00) 
b4  <- c( .707107000000E+00, -.122474500000E+01,  .000000000000E+00)
b5  <- c(-.707107000000E+00,  .408248000000E+00,  .115470100000E+01)
b6  <- c( .707107000000E+00, -.408248000000E+00, -.115470100000E+01)
b7  <- c( .141421400000E+01,  .000000000000E+00,  .000000000000E+00) 
b8  <- c(-.141421400000E+01,  .000000000000E+00,  .000000000000E+00)
b9  <- c( .707107000000E+00,  .122474500000E+01,  .000000000000E+00)
b10 <- c(-.707107000000E+00, -.122474500000E+01,  .000000000000E+00)
b11 <- c( .707107000000E+00,  .408248000000E+00,  .115470100000E+01)
b12 <- c(-.707107000000E+00, -.408248000000E+00, -.115470100000E+01)

Bmatrix <- rbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
#normalize
for (i in 1:12){
  length  <- sqrt(Bmatrix[i,1]^2 + Bmatrix[i,2]^2 + Bmatrix[i,3]^2)
  length
  Bmatrix[i,] <-  Bmatrix[i,]/length
} 

library(scatterplot3d)
scatterplot3d(x=Bmatrix[,1],y=Bmatrix[,2],z=Bmatrix[,3],pch=15,highlight.3d=TRUE)


write.table(Bmatrix,'vektoriai/A3.txt',row.names=F,col.names=F)

#### 4D
D4 = expand.grid(-1:1,-1:1,-1:1,-1:1)
D4_pre_final = D4[which(apply(abs(D4),1,sum)==2),]
dim(D4_pre_final)
D4_final = (1/sqrt(2))*D4_pre_final ## nenormalus

##D4_normal = sapply(D4_final, function(x) x/sqrt(sum(x^2))) ## normalizuoti


write.table(D4_final,'vektoriai/D4.txt',row.names=F,col.names=F)



#### 5D
D5 = expand.grid(-1:1,-1:1,-1:1,-1:1,-1:1)
D5_pre_final = D5[which(apply(abs(D5),1,sum)==2),]
dim(D5_pre_final)
D5_final = (1/sqrt(2))*D5_pre_final ## nenormalus

##D5_normal = sapply(D5_final, function(x) x/sqrt(sum(x^2))) ## normalizuoti

write.table(D5_final,'vektoriai/D5.txt',row.names=F,col.names=F)

#### 6D (5D be daugybos is saknies + velnias)
E6_1 = expand.grid(-1:1,-1:1,-1:1,-1:1,-1:1,0)
E6_pre_final_1 = E6_1[which(apply(abs(E6_1),1,sum)==2),]

E6_2 = expand.grid(c(-0.5,0.5),
                 c(-0.5,0.5),
                 c(-0.5,0.5),
                 c(-0.5,0.5),
                 c(-0.5,0.5),
                 c((sqrt(3)/2),-(sqrt(3)/2)))

E6_pre_final_2 = E6_2[which(apply(E6_2,1, function(x) sum(x<0)) %% 2 != 0),]
dim(E6_pre_final_2)

E6_final = rbind(E6_pre_final_1, E6_pre_final_2) ## nenormalus
dim(E6_final)



for (i in 1:72){
  length = sqrt(E6_final[i,1]^2 + E6_final[i,2]^2 + E6_final[i,3]^2 +
                  E6_final[i,4]^2 + E6_final[i,5]^2 + E6_final[i,6]^2)
  length
  E6_final[i,] = E6_final[i,]/length
}

##E6_normal = sapply(E6_final, function(x) x/sqrt(sum(x^2))) ## normalizuoti


write.table(E6_final,'vektoriai/E6.txt',row.names=F,col.names=F)

#### 7D
E7_1 = expand.grid(-1:1,-1:1,-1:1,-1:1,-1:1,-1:1,0)
E7_pre_final_1 = E7_1[which(apply(abs(E7_1),1,sum)==2),]
dim(E7_pre_final_1)

E7_2 = expand.grid(c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c((sqrt(2)/2),-(sqrt(2)/2)))

E7_pre_final_2 = E7_2[which(apply(E7_2,1, function(x) sum(x<0)) %% 2 != 0),]
dim(E7_pre_final_2)

E7_pre_final_3 = expand.grid(0,0,0,0,0,0,c(sqrt(2),-sqrt(2)))
dim(E7_pre_final_3)

E7_final = rbind(E7_pre_final_1,E7_pre_final_2,E7_pre_final_3) ## nenormalus
dim(E7_final)

for (i in 1:126){
  length = sqrt(E7_final[i,1]^2 + E7_final[i,2]^2 + E7_final[i,3]^2 +
                  E7_final[i,4]^2 + E7_final[i,5]^2 + E7_final[i,6]^2 + E7_final[i,7]^2)
  length
  E7_final[i,] = E7_final[i,]/length
}



###E7_normal = sapply(E7_final, function(x) x/sqrt(sum(x^2))) ## normalizuoti

write.table(E7_final,'vektoriai/E7.txt',row.names=F,col.names=F)



#### 8D
E8_1 = expand.grid(-1:1,-1:1,-1:1,-1:1,-1:1,-1:1,-1:1,-1:1)
E8_pre_final_1 = E8_1[which(apply(abs(E8_1),1,sum)==2),]
dim(E8_pre_final_1)


E8_2 = expand.grid(c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5),
                   c(-0.5,0.5))
E8_pre_final_2 = E8_2[which(apply(E8_2,1, function(x) sum(x<0)) %% 2 == 0),]
dim(E8_pre_final_2)

E8_final = rbind(E8_pre_final_1,E8_pre_final_2) ## nenormalus
dim(E8_final)


for (i in 1:240){
  length = sqrt(E8_final[i,1]^2 + E8_final[i,2]^2 + E8_final[i,3]^2 +
                E8_final[i,4]^2 + E8_final[i,5]^2 + E8_final[i,6]^2 +
                E8_final[i,7]^2 + E8_final[i,8]^2)
  length
  E8_final[i,] = E8_final[i,]/length
}


##E8_normal = sapply(E8_final, function(x) x/sqrt(sum(x^2))) ## normalizuoti

write.table(E8_final,'vektoriai/E8.txt',row.names=F,col.names=F)

####################################### 16, 24 dimensijom

####D16

output = matrix(data=0, ncol = 16)


for (i in 1:16){
  for(j in i+1:16){
    v = c(rep(0,16))  
    m = c(rep(0,16))
    p = c(rep(0,16))
    r = c(rep(0,16))
    v[i] = 1
    v[j] = 1
    m[i] = -1
    m[j] = -1
    p[i] = 1
    p[j] = -1
    r[i] = -1
    r[j] = 1
    
    output =  rbind(output,v,m,p,r)
  } 
}


D16_pre_final = output[which(apply(abs(output),1,sum)==2),]
D16_final = (1/sqrt(2))*D16_pre_final
###D16_normal = apply(D16_final,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti


#dim(D16_normal)

write.table(D16_final,'vektoriai/D16.txt',row.names=F,col.names=F)



####D24

output24 = matrix(data=0, ncol = 24)


for (i in 1:24){
  for(j in i+1:24){
    v = c(rep(0,24))  
    m = c(rep(0,24))
    p = c(rep(0,24))
    r = c(rep(0,24))
    v[i] = 1
    v[j] = 1
    m[i] = -1
    m[j] = -1
    p[i] = 1
    p[j] = -1
    r[i] = -1
    r[j] = 1
    output24 = rbind(output24,v,m,p,r)
  } 
}


D24_pre_final = output24[which(apply(abs(output24),1,sum)==2),]
D24_final = (1/sqrt(2))*D24_pre_final
##D24_normal = apply(D24_final,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti


#dim(D24_normal)

write.table(D24_final,'vektoriai/D24.txt',row.names=F,col.names=F)


####R16

R16 = matrix(data=0, ncol = 16)
for (i in 1:16){
  for(j in i+1:16){
    p = c(rep(0,16))
    r = c(rep(0,16))
    p[i] = 1
    p[j] = -1
    r[i] = -1
    r[j] = 1
    R16 = rbind(R16,p,r)
  } 
}

R16_pre_final = R16[which(apply(abs(R16),1,sum)==2),]
R16_final = (1/sqrt(2))*R16_pre_final
###R16_normal = apply(R16_final,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti


#dim(R16_normal)

write.table(R16_final,'vektoriai/R16.txt',row.names=F,col.names=F)


####R24

R24 = matrix(data=0, ncol = 25)
for (i in 1:25){
  for(j in i+1:25){
    p = c(rep(0,25))
    r = c(rep(0,25))
    p[i] = 1
    p[j] = -1
    r[i] = -1
    r[j] = 1
    R24 = rbind(R24,p,r)
  } 
}

R24_pre_final = R24[which(apply(abs(R24),1,sum)==2),]
R24_final = (1/sqrt(2))*R24_pre_final
###R24_normal = apply(R24_final,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti


#dim(R24_normal)

write.table(R24_final,'vektoriai/R24.txt',row.names=F,col.names=F)


####Z16

Z16 = rbind(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            c(-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

for (i in 2:16){
  v = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  m = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  v[i] = 1
  m[i] = -1
  Z16 = rbind(Z16,v,m)
}


###Z16_normal = apply(Z16,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti
#dim(Z16_normal)
write.table(Z16,'vektoriai/Z16.txt',row.names=F,col.names=F)

####Z24

Z24 = rbind(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            c(-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

for (i in 2:24){
  v = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  m = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  v[i] = 1
  m[i] = -1
  Z24 = rbind(Z24,v,m)
}


###Z24_normal = apply(Z24,1, function(x) x/sqrt(sum(x^2))) ## normalizuoti
#dim(Z24_normal)
write.table(Z24,'vektoriai/Z24.txt',row.names=F,col.names=F)


