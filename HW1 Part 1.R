library(ggplot2)
library(ggExtra)
library(stats)
library(plotly)
library(ash)
library(dplyr)

#part1.1
n = 100000
X1 = rnorm(n, 0, 1)
Y1 = X1 ^ 3 + rnorm(n, 0, 1)
data1 = data.frame(X1, Y1)

p1 <- ggplot(data1, aes(x=X1, y=Y1)) + geom_point() +
  ggtitle("scatter plot of data, part1")
ggMarginal(p1, type="histogram")


ggplot(data1, aes(x = X1, y = Y1)) + 
  stat_bin2d(aes(fill = ..density..), bins = 100) +
  scale_fill_viridis_c() + ggtitle("P(X1, Y1)")

jbins <- bin2(cbind(X1,Y1), nbin = c(100,100))
jf <- ash2(jbins, m = c(1,1))
pxy = matrix(jbins$nc/ n,nrow = length(jf$y),byrow = TRUE)
plot_ly(x = jf$x, y= jf$y, z = pxy, type = "contour") %>%
  layout(title = "P(X1,Y1)",
         xaxis = list(title = "X1", range = c(-2.5,2.5)) ,
         yaxis = list(title = "Y1", range = c(-11,11)))

conyx = sweep(pxy,2,colSums(pxy),`/`)
plot_ly(x = jf$x, y= jf$y, z = conyx, type = "contour")%>%
  layout(title = "P(Y1|X1)",
         xaxis = list(title = "X1", range = c(-3.5,3.5)) ,
         yaxis = list(title = "Y1", range = c(-40,40)))
pyx = t(pxy)
conxy = sweep(pyx,2,colSums(pyx),`/`)
conxy[is.nan(conxy)] = 0
plot_ly(x = jf$y, y= jf$x, z = conxy, type = "contour")%>%
  layout(title = "P(X1|Y1)",
         xaxis = list(title = "Y1" ,range = c(-50,50)) ,
         yaxis = list(title = "X1" , range = c(-4,4)))

dat1 = data.frame(x=Y1[abs(X1 - 1)< 0.01] , group="Y1|X1=1")
dat2 = data.frame(x=Y1[abs(X1 + 1)< 0.01] , group="Y1|X1=-1")
dat3 = data.frame(x=Y1[abs(X1 - 2)< 0.01], group="Y1|X1=2")
dat4 = data.frame(x=Y1[abs(X1 + 2)< 0.01] , group="Y1|X1=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="y")


dat1 = data.frame(x=X1[abs(Y1 - 1)< 0.01] , group="X1|Y1=1")
dat2 = data.frame(x=X1[abs(Y1 + 1)< 0.01] , group="X1|Y1=-1")
dat3 = data.frame(x=X1[abs(Y1 - 10)< 0.01], group="X1|Y1=10")
dat4 = data.frame(x=X1[abs(Y1 + 10)< 0.01] , group="X1|Y1=-10")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.1), breaks=seq(-5,5,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  labs(y="probability", x="x")

dox = seq(-5, 5, 0.05)
l = 1000
ydox = numeric(length(dox)* l)
xdox = numeric(length(dox)* l)
for (i in 1:(length(dox))){
  ydox[seq(l*(i-1)+1 , l*i)] = dox[i]^3 + rnorm(l)
  xdox[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * dox[i]
}

doxbins <- bin2(cbind(xdox,ydox), nbin = c(100,100))
doxf <- ash2(doxbins, m = c(1,1))
pdox = matrix(doxbins$nc, nrow = length(doxf$y) , byrow = TRUE)
pdox = sweep(pdox,2,colSums(pdox),`/`)
plot_ly(x = doxf$x, y= doxf$y, z = pdox, type = "contour")%>%
  layout(title = "P(Y1) do(X1)",
         xaxis = list(title = "X1" ,range = c(-4,4)) ,
         yaxis = list(title = "Y1" , range = c(-50,50)))


doy = seq(-10, 10, 0.05)
l = 1000
xdoy = numeric(length(dox)* l)
ydoy = numeric(length(dox)* l)
for (i in 1:(length(doy))){
  ydoy[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * doy[i]
  xdoy[seq(l*(i-1)+1 , l*i)] = rnorm(l)
}

doybins <- bin2(cbind(xdoy,ydoy), nbin = c(100,100))
doyf <- ash2(doybins, m = c(1,1))
pdoy = matrix(doybins$nc, nrow = length(doyf$y) , byrow = TRUE)
pdoy = sweep(t(pdoy),2,colSums(t(pdoy)),`/`)
plot_ly(x = doyf$y, y= doyf$x, z = pdoy, type = "contour")%>%
  layout(title = "P(X1) do(Y1)",
         xaxis = list(title = "Y1" ,range = c(-10,10)) ,
         yaxis = list(title = "X1" , range = c(-4,4)))

dat1dox = data.frame(x=ydox[xdox ==1] , group="p(Y) do(X=1)")
dat2dox = data.frame(x=ydox[xdox ==-1] , group="p(Y) do(X=-1)")
dat3dox = data.frame(x=ydox[xdox ==2] , group="p(Y) do(X=2)")
dat4dox = data.frame(x=ydox[xdox ==-2] , group="p(Y) do(X=-2)")
ggplot(rbind(dat1dox,dat2dox,dat3dox,dat4dox), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
   labs(y="probability", x="x")


dat1doy = data.frame(x=xdoy[ydoy ==1] , group="p(X) do(Y=1)")
dat2doy = data.frame(x=xdoy[ydoy ==-1] , group="p(X) do(Y=-1)")
dat3doy = data.frame(x=xdoy[ydoy ==10] , group="p(X) do(Y=10)")
dat4doy = data.frame(x=xdoy[ydoy ==-10] , group="p(X) do(Y=-10)")
ggplot(rbind(dat1doy,dat2doy,dat3doy,dat4doy), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) + labs(y="probability", x="x")

#part1.2
n = 100000
X2 = rnorm(n, 0, 1)
Y2 = 2 * X2 + rt(n, 1)
data2 = data.frame(X2, Y2)

p2 <- ggplot(data2, aes(x=X2, y=Y2)) + geom_point() +
  ggtitle("scatter plot of data, part2")
ggMarginal(p2, type="histogram")
View(p2)

Y2[abs(Y2) > 1500] = 0
data2 = data.frame(X2, Y2)
ggplot(data2, aes(x = X2, y = Y2)) + 
  stat_bin2d(aes(fill = ..density..), bins = 100) +
  scale_fill_viridis_c() + ggtitle("P(X2, Y2)")


jbins <- bin2(cbind(X2,Y2), nbin = c(100,4000))
jf <- ash2(jbins, m = c(1,1))
pxy = matrix(jbins$nc/ n,nrow = length(jf$y),byrow = TRUE)
plot_ly(x = jf$x, y= jf$y, z = pxy, type = "contour") %>%
  layout(title = "P(X2,Y2)",
         xaxis = list(title = "X2", range = c(-3,3)) ,
         yaxis = list(title = "Y2", range = c(-10,10)))

conyx = sweep(pxy,2,colSums(pxy),`/`)
plot_ly(x = jf$x, y= jf$y, z = conyx, type = "contour")%>%
  layout(title = "P(Y2|X2)",
         xaxis = list(title = "X2", range = c(-4,4)) ,
         yaxis = list(title = "Y2", range = c(-50,50)))

pyx = t(pxy)
conxy = sweep(pyx,2,colSums(pyx),`/`)
conxy[is.nan(conxy)] = 0
plot_ly(x = jf$y, y= jf$x, z = conxy, type = "contour")%>%
  layout(title = "P(X2|Y2)",
         xaxis = list(title = "Y2") ,
         yaxis = list(title = "X2"))

dat1 = data.frame(x=Y2[abs(X2 - 1)< 0.01] , group="Y2|X2=1")
dat2 = data.frame(x=Y2[abs(X2 + 1)< 0.01] , group="Y2|X2=-1")
dat3 = data.frame(x=Y2[abs(X2 - 2)< 0.01], group="Y2|X2=2")
dat4 = data.frame(x=Y2[abs(X2 + 2)< 0.01] , group="Y2|X2=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="y")


dat1 = data.frame(x=X2[abs(Y2 - 1)< 0.01] , group="X2|Y2=1")
dat2 = data.frame(x=X2[abs(Y2 + 1)< 0.01] , group="X2|Y2=-1")
dat3 = data.frame(x=X2[abs(Y2 - 10)< 0.01], group="X2|Y2=10")
dat4 = data.frame(x=X2[abs(Y2 + 10)< 0.01] , group="X2|Y2=-10")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.1), breaks=seq(-5,5,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  labs(y="probability", x="x")

dox = seq(-5, 5, 0.05)
l = 1000
ydox = numeric(length(dox)* l)
xdox = numeric(length(dox)* l)
for (i in 1:(length(dox))){
  ydox[seq(l*(i-1)+1 , l*i)] = dox[i] * 2 + rt(l,1)
  xdox[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * dox[i]
}

doxbins <- bin2(cbind(xdox,ydox), nbin = c(100,10000))
doxf <- ash2(doxbins, m = c(1,1))
pdox = matrix(doxbins$nc, nrow = length(doxf$y) , byrow = TRUE)
pdox = sweep(pdox,2,colSums(pdox),`/`)
plot_ly(x = doxf$x, y= doxf$y, z = pdox, type = "contour")%>%
  layout(title = "P(Y2) do(X2)",
         xaxis = list(title = "X2" ,range = c(-4,4)) ,
         yaxis = list(title = "Y2" , range = c(-100,100)))


doy = seq(-10, 10, 0.05)
l = 1000
xdoy = numeric(length(dox)* l)
ydoy = numeric(length(dox)* l)
for (i in 1:(length(doy))){
  ydoy[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * doy[i]
  xdoy[seq(l*(i-1)+1 , l*i)] = rnorm(l)
}

doybins <- bin2(cbind(xdoy,ydoy), nbin = c(100,100))
doyf <- ash2(doybins, m = c(1,1))
pdoy = matrix(doybins$nc, nrow = length(doyf$y) , byrow = TRUE)
pdoy = sweep(t(pdoy),2,colSums(t(pdoy)),`/`)
plot_ly(x = doyf$y, y= doyf$x, z = pdoy, type = "contour")%>%
  layout(title = "P(X2) do(Y2)",
         xaxis = list(title = "Y2" ,range = c(-10,10)) ,
         yaxis = list(title = "X2" , range = c(-4,4)))

dat1dox = data.frame(x=ydox[xdox ==1] , group="p(Y) do(X=1)")
dat2dox = data.frame(x=ydox[xdox ==-1] , group="p(Y) do(X=-1)")
dat3dox = data.frame(x=ydox[xdox ==2] , group="p(Y) do(X=2)")
dat4dox = data.frame(x=ydox[xdox ==-2] , group="p(Y) do(X=-2)")
ggplot(rbind(dat1dox,dat2dox,dat3dox,dat4dox), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="x")


dat1doy = data.frame(x=xdoy[ydoy ==1] , group="p(X) do(Y=1)")
dat2doy = data.frame(x=xdoy[ydoy ==-1] , group="p(X) do(Y=-1)")
dat3doy = data.frame(x=xdoy[ydoy ==10] , group="p(X) do(Y=10)")
dat4doy = data.frame(x=xdoy[ydoy ==-10] , group="p(X) do(Y=-10)")
ggplot(rbind(dat1doy,dat2doy,dat3doy,dat4doy), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) + labs(y="probability", x="x")


#part1.3
n = 100000
X3 = rnorm(n, 0, 1)
Y3 = 2 * X3 + rt(n, 5)
data3 = data.frame(X3, Y3)

p3 <- ggplot(data3, aes(x=X3, y=Y3)) + geom_point() +
  ggtitle("scatter plot of data, part3")
ggMarginal(p3, type="histogram")
View(p3)

ggplot(data3, aes(x = X3, y = Y3)) + 
  stat_bin2d(aes(fill = ..density..), bins = 100) +
  scale_fill_viridis_c() + ggtitle("P(X3, Y3)")


jbins <- bin2(cbind(X3,Y3), nbin = c(100,100))
jf <- ash2(jbins, m = c(1,1))
pxy = matrix(jbins$nc/ n,nrow = length(jf$y),byrow = TRUE)
plot_ly(x = jf$x, y= jf$y, z = pxy, type = "contour") %>%
  layout(title = "P(X3,Y3)",
         xaxis = list(title = "X3", range = c(-5,5)) ,
         yaxis = list(title = "Y3", range = c(-10,10)))

conyx = sweep(pxy,2,colSums(pxy),`/`)
plot_ly(x = jf$x, y= jf$y, z = conyx, type = "contour")%>%
  layout(title = "P(Y3|X3)",
         xaxis = list(title = "X3", range = c(-4,4)) ,
         yaxis = list(title = "Y3", range = c(-10,10)))

pyx = t(pxy)
conxy = sweep(pyx,2,colSums(pyx),`/`)
conxy[is.nan(conxy)] = 0
plot_ly(x = jf$y, y= jf$x, z = conxy, type = "contour")%>%
  layout(title = "P(X3|Y3)",
         xaxis = list(title = "Y3") ,
         yaxis = list(title = "X3"))

dat1 = data.frame(x=Y3[abs(X3 - 1)< 0.01] , group="Y3|X3=1")
dat2 = data.frame(x=Y3[abs(X3 + 1)< 0.01] , group="Y3|X3=-1")
dat3 = data.frame(x=Y3[abs(X3 - 2)< 0.01], group="Y3|X3=2")
dat4 = data.frame(x=Y3[abs(X3 + 2)< 0.01] , group="Y3|X3=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="y")


dat1 = data.frame(x=X3[abs(Y3 - 1)< 0.01] , group="X3|Y3=1")
dat2 = data.frame(x=X3[abs(Y3 + 1)< 0.01] , group="X3|Y3=-1")
dat3 = data.frame(x=X3[abs(Y3 - 2)< 0.01], group="X3|Y3=2")
dat4 = data.frame(x=X3[abs(Y3 + 2)< 0.01] , group="X3|Y3=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.1), breaks=seq(-5,5,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  labs(y="probability", x="x")

dox = seq(-5, 5, 0.05)
l = 1000
ydox = numeric(length(dox)* l)
xdox = numeric(length(dox)* l)
for (i in 1:(length(dox))){
  ydox[seq(l*(i-1)+1 , l*i)] = dox[i] * 2 + rt(l,5)
  xdox[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * dox[i]
}

doxbins <- bin2(cbind(xdox,ydox), nbin = c(100,100))
doxf <- ash2(doxbins, m = c(1,1))
pdox = matrix(doxbins$nc, nrow = length(doxf$y) , byrow = TRUE)
pdox = sweep(pdox,2,colSums(pdox),`/`)
plot_ly(x = doxf$x, y= doxf$y, z = pdox, type = "contour")%>%
  layout(title = "P(Y3) do(X3)",
         xaxis = list(title = "X3" ,range = c(-4,4)) ,
         yaxis = list(title = "Y3" , range = c(-20,20)))


doy = seq(-10, 10, 0.05)
l = 1000
xdoy = numeric(length(dox)* l)
ydoy = numeric(length(dox)* l)
for (i in 1:(length(doy))){
  ydoy[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * doy[i]
  xdoy[seq(l*(i-1)+1 , l*i)] = rnorm(l)
}

doybins <- bin2(cbind(xdoy,ydoy), nbin = c(100,100))
doyf <- ash2(doybins, m = c(1,1))
pdoy = matrix(doybins$nc, nrow = length(doyf$y) , byrow = TRUE)
pdoy = sweep(t(pdoy),2,colSums(t(pdoy)),`/`)
plot_ly(x = doyf$y, y= doyf$x, z = pdoy, type = "contour")%>%
  layout(title = "P(X3) do(Y3)",
         xaxis = list(title = "Y3" ,range = c(-10,10)) ,
         yaxis = list(title = "X3" , range = c(-4,4)))

dat1dox = data.frame(x=ydox[xdox ==1] , group="p(Y) do(X=1)")
dat2dox = data.frame(x=ydox[xdox ==-1] , group="p(Y) do(X=-1)")
dat3dox = data.frame(x=ydox[xdox ==2] , group="p(Y) do(X=2)")
dat4dox = data.frame(x=ydox[xdox ==-2] , group="p(Y) do(X=-2)")
ggplot(rbind(dat1dox,dat2dox,dat3dox,dat4dox), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="x")


dat1doy = data.frame(x=xdoy[ydoy ==1] , group="p(X) do(Y=1)")
dat2doy = data.frame(x=xdoy[ydoy ==-1] , group="p(X) do(Y=-1)")
dat3doy = data.frame(x=xdoy[ydoy ==10] , group="p(X) do(Y=10)")
dat4doy = data.frame(x=xdoy[ydoy ==-10] , group="p(X) do(Y=-10)")
ggplot(rbind(dat1doy,dat2doy,dat3doy,dat4doy), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) + labs(y="probability", x="x")

#part1.4
n = 100000
X4 = rnorm(n, 0, 1)
Y4 = 2 * X4 + rt(n, 20)
data4 = data.frame(X4, Y4)

p4 <- ggplot(data4, aes(x=X4, y=Y4)) + geom_point() +
  ggtitle("scatter plot of data, part4")
ggMarginal(p4, type="histogram")
View(p4)

ggplot(data4, aes(x = X4, y = Y4)) + 
  stat_bin2d(aes(fill = ..density..), bins = 100) +
  scale_fill_viridis_c() + ggtitle("P(X4, Y4)")


jbins <- bin2(cbind(X4,Y4), nbin = c(100,100))
jf <- ash2(jbins, m = c(1,1))
pxy = matrix(jbins$nc/ n,nrow = length(jf$y),byrow = TRUE)
plot_ly(x = jf$x, y= jf$y, z = pxy, type = "contour") %>%
  layout(title = "P(X4,Y4)",
         xaxis = list(title = "X4", range = c(-5,5)) ,
         yaxis = list(title = "Y4", range = c(-10,10)))

conyx = sweep(pxy,2,colSums(pxy),`/`)
plot_ly(x = jf$x, y= jf$y, z = conyx, type = "contour")%>%
  layout(title = "P(Y4|X4)",
         xaxis = list(title = "X4", range = c(-4,4)) ,
         yaxis = list(title = "Y4", range = c(-10,10)))

pyx = t(pxy)
conxy = sweep(pyx,2,colSums(pyx),`/`)
conxy[is.nan(conxy)] = 0
plot_ly(x = jf$y, y= jf$x, z = conxy, type = "contour")%>%
  layout(title = "P(X4|Y4)",
         xaxis = list(title = "Y4") ,
         yaxis = list(title = "X4"))

dat1 = data.frame(x=Y4[abs(X4 - 1)< 0.01] , group="Y4|X4=1")
dat2 = data.frame(x=Y4[abs(X4 + 1)< 0.01] , group="Y4|X4=-1")
dat3 = data.frame(x=Y4[abs(X4 - 2)< 0.01], group="Y4|X4=2")
dat4 = data.frame(x=Y4[abs(X4 + 2)< 0.01] , group="Y4|X4=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="y")


dat1 = data.frame(x=X4[abs(Y4 - 1)< 0.01] , group="X4|Y4=1")
dat2 = data.frame(x=X4[abs(Y4 + 1)< 0.01] , group="X4|Y4=-1")
dat3 = data.frame(x=X4[abs(Y4 - 2)< 0.01], group="X4|Y4=2")
dat4 = data.frame(x=X4[abs(Y4 + 2)< 0.01] , group="X4|Y4=-2")
ggplot(rbind(dat1,dat2,dat3,dat4), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.1), breaks=seq(-5,5,0.1), alpha=0.6, 
                 position="identity", lwd=0.2) +
  labs(y="probability", x="x")

dox = seq(-5, 5, 0.05)
l = 1000
ydox = numeric(length(dox)* l)
xdox = numeric(length(dox)* l)
for (i in 1:(length(dox))){
  ydox[seq(l*(i-1)+1 , l*i)] = dox[i] * 2 + rt(l,20)
  xdox[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * dox[i]
}

doxbins <- bin2(cbind(xdox,ydox), nbin = c(100,100))
doxf <- ash2(doxbins, m = c(1,1))
pdox = matrix(doxbins$nc, nrow = length(doxf$y) , byrow = TRUE)
pdox = sweep(pdox,2,colSums(pdox),`/`)
plot_ly(x = doxf$x, y= doxf$y, z = pdox, type = "contour")%>%
  layout(title = "P(Y4) do(X4)",
         xaxis = list(title = "X4" ,range = c(-4,4)) ,
         yaxis = list(title = "Y4" , range = c(-10,10)))


doy = seq(-10, 10, 0.05)
l = 1000
xdoy = numeric(length(dox)* l)
ydoy = numeric(length(dox)* l)
for (i in 1:(length(doy))){
  ydoy[seq(l*(i-1)+1 , l*i)] = (1 - numeric(l)) * doy[i]
  xdoy[seq(l*(i-1)+1 , l*i)] = rnorm(l)
}

doybins <- bin2(cbind(xdoy,ydoy), nbin = c(100,100))
doyf <- ash2(doybins, m = c(1,1))
pdoy = matrix(doybins$nc, nrow = length(doyf$y) , byrow = TRUE)
pdoy = sweep(t(pdoy),2,colSums(t(pdoy)),`/`)
plot_ly(x = doyf$y, y= doyf$x, z = pdoy, type = "contour")%>%
  layout(title = "P(X4) do(Y4)",
         xaxis = list(title = "Y4" ,range = c(-10,10)) ,
         yaxis = list(title = "X4" , range = c(-4,4)))

dat1dox = data.frame(x=ydox[xdox ==1] , group="p(Y) do(X=1)")
dat2dox = data.frame(x=ydox[xdox ==-1] , group="p(Y) do(X=-1)")
dat3dox = data.frame(x=ydox[xdox ==2] , group="p(Y) do(X=2)")
dat4dox = data.frame(x=ydox[xdox ==-2] , group="p(Y) do(X=-2)")
ggplot(rbind(dat1dox,dat2dox,dat3dox,dat4dox), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) +
  labs(y="probability", x="x")


dat1doy = data.frame(x=xdoy[ydoy ==1] , group="p(X) do(Y=1)")
dat2doy = data.frame(x=xdoy[ydoy ==-1] , group="p(X) do(Y=-1)")
dat3doy = data.frame(x=xdoy[ydoy ==10] , group="p(X) do(Y=10)")
dat4doy = data.frame(x=xdoy[ydoy ==-10] , group="p(X) do(Y=-10)")
ggplot(rbind(dat1doy,dat2doy,dat3doy,dat4doy), aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..*0.2), breaks=seq(-13,13,0.2), alpha=0.6, 
                 position="identity", lwd=0.5) + labs(y="probability", x="x")

