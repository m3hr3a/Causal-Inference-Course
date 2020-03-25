library(minpack.lm)
library(kpcalg)
library(pracma)

getDirection <- function(X, Y, z0,z1) {
    m1 <- nls(Y ~ a + b * I(X^z0), start = list(a = 2, b = 1))
    res1 = Y - fitted(m1)
    p1 = hsic.perm(X,res1)$p.value
    m2 <- nls(X ~ a + b * nthroot(Y,z1), start = list(a = -10, b = 1))
    res2 = X - fitted(m2)
    p2 = hsic.perm(Y,res2)$p.value
    if (p1 >= p2){
      direction = 1
    }else {
      direction = 2
    }
  return(direction)
}

n = 200
direction1 = numeric(10)
for (i in 1 : 10){
  X1 = rnorm(n, 0, 1)
  Y1 = X1 ^ 3 + rnorm(n, 0, 1)
  direction1[i] = getDirection(X1, Y1, 3, 3)
}
plot(direction1, main = "part1 , 1 : X->Y, 2 : Y -> X", xlab = "iteration")

m1 <- nls(Y1 ~ a + b * I(X1^3), start = list(a = 2, b = 1))
res1 = Y1 - fitted(m1)
m2 <- nls(X1 ~ a + b * nthroot(Y1,3), start = list(a = -10, b = 1))
res2 = X1 - fitted(m2)
plot(X1, res1, main = "residual for model 1 step 2")
plot(Y1, res2, main = "residual for model 1 step 4")
direction2 = numeric(10)
for (i in 1 : 10){
  X2 = rnorm(n, 0, 1)
  Y2 = 2 * X2 + rt(n,1)
  direction2[i] = getDirection(X2, Y2, 1, 1)
}
plot(direction2, main = "part2 , 1 : X->Y, 2 : Y -> X", xlab = "iteration")

direction3 = numeric(10)
for (i in 1 : 10){
  X3 = rnorm(n, 0, 1)
  Y3 = 2 * X3 + rt(n,5)
  direction3[i] = getDirection(X3, Y3, 1, 1)
}
plot(direction3, main = "part3 , 1 : X->Y, 2 : Y -> X")

direction4 = numeric(10)
for (i in 1 : 10){
  X4 = rnorm(n, 0, 1)
  Y4 = 2 * X4 + rt(n,20)
  direction4[i] = getDirection(X4, Y4, 1, 1)
}
plot(direction4, main = "part4 , 1 : X->Y, 2 : Y -> X")


meta <- read.table("pairmeta.txt", colClasses = c("character", "numeric",
                                                  "numeric","numeric",
                                                  "numeric","numeric"))

direction = numeric(dim(meta[1])[1])
#direction = readRDS("direc.rds")

for (i in 1 : dim(meta[1])[1]){
  print(i)
  if (i != 17 & i!= 68 & i != 74 & i!= 75){
    if ((meta[i,2] == meta[i,3]) && (meta[i,4] == meta[i,5])){
      data <- read.table(sprintf("pair%s.txt",meta[i,1]))
      cause = data[,meta[i,2]]
      effect = data[,meta[i,4]]
      direction[i] = getDirection(cause, effect, 1, 1)
    } 
  }
}
hdd = 1; 
k = 1; 
for (i in 1 : dim(meta[1])[1]){
  if ((meta[i,2] == meta[i,3]) && (meta[i,4] == meta[i,5])){
    
  } else {
    hdd[k] = i; 
    k = k + 1;
  }
}



direction52=0
data <- read.table(sprintf("pair0052.txt",meta[i,1]))
cause = data[,meta[52,2]:meta[52,3]]
effect = data[,meta[52,4]:meta[52,5]]
for (i in 1:4){
  for (j in 1:4){
    c = cause[,i]
    e = effect[,j]
    direction52[(i-1)*4+j] = getDirection(c, e, 1, 1)
  }
}

direction53=0
data <- read.table(sprintf("pair0053.txt",meta[i,1]))
cause = data[,meta[53,2]:meta[53,3]]
effect = data[,meta[53,4]:meta[53,5]]
for (i in 1:3){
  c = cause[,i]
  e = effect
  direction53[i] = getDirection(c, e, 1, 1)
}

direction[53] = mean(direction53)

direction54=0
data <- read.table(sprintf("pair0054.txt",meta[i,1]))
cause = data[,meta[54,2]:meta[54,3]]
effect = data[,meta[54,4]:meta[54,5]]
for (i in 1:3){
  for (j in 1:2){
    c = cause[,i]
    e = effect[,j]
    direction54[(i-1)*2+j] = getDirection(c, e, 1, 1)
  }
}
direction[54] = mean(direction54)

direction55=0
data <- read.table(sprintf("pair0055.txt",meta[i,1]))
cause = data[,meta[55,2]:meta[55,3]]
effect = data[,meta[55,4]:meta[55,5]]
for (i in 1:16){
  for (j in 1:16){
    c = cause[,i]
    e = effect[,j]
    direction55[(i-1)*16+j] = getDirection(c, e, 1, 1)
  }
}
direction[55] = 1*(mean(direction55) < 1.5 +1)

direction71=0
data <- read.table(sprintf("pair0071.txt",meta[i,1]))
cause = data[,meta[71,2]:meta[71,3]]
effect = data[,meta[71,4]:meta[71,5]]
for (i in 1:6){
  for (j in 1:2){
    c = cause[,i]
    e = effect[,j]
    direction71[(i-1)*2+j] = getDirection(c, e, 1, 1)
  }
}
direction[71] = 1*(mean(direction71) < 1.5 +1)


direction105=0
data <- read.table(sprintf("pair0105.txt",meta[i,1]))
cause = data[,meta[105,2]:meta[105,3]]
effect = data[,meta[105,4]:meta[105,5]]
for (i in 1:9){
    c = cause[,i]
    e = effect
    direction105[i] = getDirection(c, e, 1, 1)
}
direction[105] = 1*(mean(direction105) < 1.5 +1)

plot(direction, main="0 : no result, 1 : True direction, 2 : False direction")



#error data 106
data <- read.table("pair0106.txt")
X = data[,meta[106,2]]
Y = data[,meta[106,4]]
plot(X,Y, main = "106th data")
m1 <- nls(Y ~ a + b * I(X^1), start = list(a = 2, b = 1))
res1 = Y - fitted(m1)
p1 = hsic.perm(X,res1)$p.value
m2 <- nls(X ~ a + b * nthroot(Y,1), start = list(a = -10, b = 1))
res2 = X - fitted(m2)
p2 = hsic.perm(Y,res2)$p.value
plot(X,res1)
plot(Y,res2)
