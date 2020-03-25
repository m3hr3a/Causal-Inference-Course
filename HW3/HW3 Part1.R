library(NlinTS)
library(pcalg)

my_data <- read.table("model01.txt", sep = ",")
ground_truth = read.table("graphModel01.txt", sep = ",")
X = my_data$V1
Y = my_data$V2
Z = my_data$V3

X_1 = X
X_1[1] = 0
X_1[2:1000] = X[1:999]
Y_1 = Y
Y_1[1] = 0
Y_1[2:1000] = Y[1:999]
Z_1 = Z
Z_1[1] = 0
Z_1[2:1000] = Z[1:999]
data = cbind(X,Y,Z,X_1,Y_1,Z_1)
labs = c("X","Y","Z","X_1","Y_1","Z_1")
# auto regression
ytox = causality.test(X, Y, lag = 1)
xtoy = causality.test(Y, X, lag = 1)
ztox = causality.test(X, Z, lag = 1)
xtoz = causality.test(Z, X, lag = 1)
ztoy = causality.test(Y, Z, lag = 1)
ytoz = causality.test(Z, Y, lag = 1)
#----Graph---#
A = matrix(c(1:9) * 0, nrow = 3, ncol = 3)
sig = 0.05
if (xtoy$pvalue < sig){
  A[1,2] = 1
} 
if (ytox$pvalue < sig) {
  A[2,1] = 1
}
if (xtoz$pvalue < sig){
  A[1,3] = 1
} 
if (ztox$pvalue < sig) {
  A[3,1] = 1
}
if (ytoz$pvalue < sig){
  A[2,3] = 1
} 
if (ztoy$pvalue < sig) {
  A[3,2] = 1
}
G1 = graph_from_adjacency_matrix(A)
plot(G1)

m1 = ar(cbind(X,Y,Z), lag = 1)
pred = m1$ar[1,,]
cor(c(as.matrix(ground_truth)), c(as.matrix(pred)))

# pc
pcModel = pc(suffStat = list(C = cor(data), n = 1000),
             indepTest = gaussCItest, ## indep.test: partial correlations
             alpha=0.01, labels = labs , verbose = TRUE)
mod1 = lm(X ~ cbind(X_1, Y_1, Z_1))
mod2 = lm(Y ~ cbind(X_1, Y_1, Z_1))
mod3 = lm(Z ~ cbind(X_1, Y_1, Z_1))
A[1,] = mod1$coefficients[2:4]
A[2,] = mod2$coefficients[2:4]
A[3,] = mod3$coefficients[2:4]
cor(c(as.matrix(ground_truth)), c(A))
plot(pcModel)
as(pcModel, "amat")
# fci 
fciModel = fci(suffStat = list(C = cor(data), n = 1000),
             indepTest = gaussCItest, ## indep.test: partial correlations
             alpha=0.01, labels = labs , verbose = TRUE)
plot(fciModel)
as(fciModel, "amat")
# lingam
lingamModel = lingam(data, verbose = FALSE)
B = lingamModel$Bpruned
A = B[1:3,4:6]
cor(c(as.matrix(ground_truth)), c(A))
# model2
my_data <- read.table("model02.txt", sep = ",")
X = my_data$V1
Y = my_data$V2
Z = my_data$V3
m2 = ar(cbind(X,Y,Z), lag = 1)
pred2 = m2$ar[1,,]
write.table(pred2, file="graphModel02.txt", row.names=FALSE, col.names=FALSE, sep = ",")
