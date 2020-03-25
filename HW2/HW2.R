library(graph)
library(gtools)
library(pcalg)

isIndXY <- function(x, y, sig) {
  N = length(x)
  pxy = cor(x, y)
  z = abs(sqrt(N - 3) * 0.5 * log((1 + pxy)/(1 - pxy)))
  return(z < qnorm(1 - sig / 2))
}

isIndij <- function(i, j, X, sig) {
  x = X[,i]
  y = X[,j]
  return(isIndXY(x, y, sig))
}

isIndijk <- function(i, j, k , X, sig) {
  x = X[,i]
  y = X[,j]
  z = X[,k]
  lm1 = lm(x ~ z)
  resX = x - predict(lm1, data.frame(z))
  lm2 = lm(y ~ z)
  resY = y - predict(lm2, data.frame(z))
  return(isIndXY(resX, resY, sig))
}

myPC <- function(X, sigLevel) {
  n = dim(X)[2]
  C <- randomGraph(1 : n, 1:n, p = 1)
  l = -1
  S = matrix(list(), n, n)
  while (TRUE) {
    l = l + 1; 
    perms = permutations(n, 2, 1:n)
    for (iterator1 in 1:dim(perms)[1]) {
      i = perms[iterator1,1]
      j = perms[iterator1,2]
      adjI = adj(C, i)[[1]]
      adjI = as.numeric(adjI)
      adjI = adjI[adjI != j]
      if (length(adjI) >= l) {
        if (l == 0) {
          if (isIndij(i, j, X, sigLevel)) {
            if (j %in% sapply(adj(C, i), as.numeric)) {
              print(sprintf("removed = %d to %d", i, j))
              C = removeEdge(sprintf("%d", i), sprintf("%d", j), C)
            }
          }
        }
        else {
          subsets = combinations(length(adjI), l, 1:length(adjI))
          for (iterator2 in 1:dim(subsets)[1]) {
            k = adjI[subsets[iterator2,]]
            if (isIndijk(i, j, k, X, sigLevel)) {
              if (j %in% sapply(adj(C, i), as.numeric)) {
                print(sprintf("removed = %d to %d", i, j))
                C = removeEdge(sprintf("%d", i), sprintf("%d", j), C)
              }
              S[i,j][[1]] = unique(append(S[i,j][[1]], k))
              S[j,i][[1]] = unique(append(S[i,j][[1]], k))
            }
          }
        }
      }
    }
    if (l > n) {
      return(list(C, S))
    }
  }
}

meek <- function(C, S) {
  Cadj = 1 * (as(C, "matrix") != 0)
  n = dim(Cadj)[1]
  V = character(n)
  for (i in 1 : n) { 
    V[i] = toString(i)
  }
  
  dGraph <- graphNEL(nodes= V, edgeL=list(), edgemode="directed")
  uTr <- find.unsh.triple(Cadj)$unshTripl
  for (i in 1:dim(uTr)[2]) {
    from = uTr[,i][[1]]
    mid = uTr[,i][[2]]
    end = uTr[,i][[3]]
    if (!(mid %in% S[from, end][[1]])) {
      if (!(mid %in% sapply(adj(dGraph, from), as.numeric))) {
        dGraph = addEdge(sprintf("%d", from), sprintf("%d", mid), dGraph)
      }
      if (!(mid %in% sapply(adj(dGraph, end), as.numeric))) {
        dGraph = addEdge(sprintf("%d", end), sprintf("%d", mid), dGraph)
      }
      if (mid %in% sapply(adj(C, from), as.numeric)) {
        C = removeEdge(sprintf("%d", from), sprintf("%d", mid), C)
      }
      if (mid %in% sapply(adj(C, end), as.numeric)) {
        C = removeEdge(sprintf("%d", end), sprintf("%d", mid), C)
      }
    }
  }

  while(TRUE) {
    used = 0;
    for (j in 1:n) {
      adj1 = as.numeric(adj(dGraph, j)[[1]])
      for (k in adj1) {
        adj2 = as.numeric(adj(C, k)[[1]])
        for (m in adj2) {
          if (!(j %in% adj(C,m)[[1]]) && !(j %in% adj(dGraph,m)[[1]]) &&
              !(m %in% adj(dGraph,j)[[1]])) {
            used = 1; 
            dGraph = addEdge(sprintf("%d", k), sprintf("%d", m), dGraph)
            if (k %in% sapply(adj(C, m), as.numeric)) {
              C = removeEdge(sprintf("%d", k), sprintf("%d", m), C)
            }
          }
        }
      }
    }
    for (j in 1:n) {
      adj1 = as.numeric(adj(dGraph, j)[[1]])
      for (k in adj1) {
        adj2 = as.numeric(adj(dGraph, k)[[1]])
        for (m in adj2) {
          if ((j %in% adj(C,m)[[1]])) {
            used = 1
            dGraph = addEdge(sprintf("%d", j), sprintf("%d", m), dGraph)
            if (j %in% sapply(adj(C, m), as.numeric)) {
              C = removeEdge(sprintf("%d", j), sprintf("%d", m), C)
            }
          }
        }
      }
    }
    if (used == 0) {
      for (j in 1:n) {
        adj1 = as.numeric(adj(C, j)[[1]])
        for (k in adj1) {
          if (k %in% as.numeric(adj(C, j)[[1]])) {
            C = removeEdge(sprintf("%d", j), sprintf("%d", k), C)
            dGraph = addEdge(sprintf("%d", j), sprintf("%d", k), dGraph)
            dGraph = addEdge(sprintf("%d", k), sprintf("%d", j), dGraph)
          }
        }
      }
      return(dGraph)
    }
    
  }
}

set.seed(5)
nSample = 10000 # 100 1000
X1 = 1.2 * rnorm(nSample)
X2 = rnorm(nSample)
X3 = 2 * X1 - 0.5 * X2 + rnorm(nSample)
X4 = 0.4 * X3 + rnorm(nSample)
X5 = 0.8 * X3 - X4 + 0.6 * rnorm(nSample)
X6 = X2 + X5 + rnorm(nSample)
data1 = cbind(X1, X2, X3, X4, X5, X6)
out1 = myPC(data1, 0.05)
G1 = out1[[1]]
plot(G1)
S1 = out1[[2]]
C1 = meek(G1, S1)
plot(C1, main = sprintf("nSample = %d", nSample))


G2 = randomDAG(20, prob = 0.2, lB = 0.1, uB = 1)
plot(G2)
data2 = rmvDAG(1000, G2, errDist = "normal")
out2 = myPC(data2, 0.05)
G2hat = out2[[1]]
S2 = out2[[2]]
plot(G2hat)
C2 = meek(G2hat, S2)
plot(C2)
shd(C2,G2)
k = 1 
sgVals = c(0.000001, 0.000005, 0.00001, 0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.5)

for (sg in sgVals){
  out2 = myPC(data2, sg)
  G2hat = out2[[1]]
  S2 = out2[[2]]
  C2 = meek(G2hat, S2)
  shd(C2,G2)
  if (k == 1){
    hds = shd(C2,G2)
  } else {
    hds = append(hds, shd(C2,G2))
  }
  k = k + 1
}

plot(sgVals, hds, main ="Hamming Distance based on Sig. Level" )



k = 1 
sgVals = seq(0.000001, 0.3  ,0.001)
G3 = randomDAG(20, prob = 0.2, lB = 0.1, uB = 1)
plot(G3)
data3 = rmvDAG(1000, G3, errDist = "normal")

for (sg in sgVals){
  out2 = myPC(data3, sg)
  G2hat = out2[[1]]
  S2 = out2[[2]]
  C2 = meek(G2hat, S2)
  shd(C2,G3)
  if (k == 1){
    hds = shd(C2,G3)
  } else {
    hds = append(hds, shd(C2,G3))
  }
  k = k + 1
}

lines(sgVals[1:300], hds[1:300], main ="Hamming Distance based on Sig. Level" )
