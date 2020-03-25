library(pcalg)
library(igraph)

toChainComp <- function(G) {
  p = dim(G)[1]
  nodes = 1:p 
  for (i in nodes) {
    for (j in nodes) {
      if (G[i,j] == 1 && G[j,i] == 0){
        G[i,j] = 0
      }
    }
  }
  Graph= g1 <- graph_from_adjacency_matrix(G)
  clu <- components(Graph)
  return(groups(clu))
}

ChainCom <- function(U, v) {
  p = dim(U)[1]
  Total = 1:p
  A = list()
  A = v
  G = U
  B = Total[-v]
  while (length(B) != 0) {
    adjA = list()
    myT = list()
    for (a in A) {
      adjA = append(adjA, which(U[a,] %in% 1))
    }
    for (b in B) {
      if (b %in% adjA) {
        myT = append(myT, b)
      }
    }
    for (a in A) {
      for (t in myT) {
        G[t,a] = 0
      }
    }
    while (TRUE) {
        oriented = 0
        for (y in myT) {
          for (z in myT) {
            if (G[z,y] == 1 && G[y,z] == 1) {
              for (x in 1:p) {
                if (G[x,y] == 1 && G[y,x] == 0 && G[x,z] == 0 && G[z,x] == 0) {
                    G[z,y] = 0
                    oriented = 1
                }
              }
            }
          }
        }
      if (oriented == 0) {
        break
      }
    }
    A = myT
    B = B[!(B %in% myT)]
  }
  O = toChainComp(G)
  return(list(G, O))
}

sizeMEC <- function(U) {
  p = dim(U)[1]
  if (is.null(p)) {
    p = 1
  }
  n = sum(U) / 2
  if (n == p-1) {
    return(p)
  }
  if (p == n) {
    return(2*p)
  }
  if (n == p * (p - 1) / 2 - 2) {
    return((p^2 - p - 4)*factorial(p-3))
  }
  if (n == p*(p-1)/2 - 1) {
    return(2*factorial(p-1) - factorial(p-2))
  }
  if (n == p*(p-1)/2) {
    return(factorial(p))
  }
  st = list()
  for (j in 1:p) {
    comps = ChainCom(U, j)[[2]]; 
    s = 1; 
    for (uj in comps) {
      s = s*sizeMEC(U[as.numeric(uj), as.numeric(uj)])
    }
    j
    st[j] = s
  }
  stot = 0
  for (a in st) {
    stot = stot + a
  }
  return(stot)
}

SizeMECGraph <- function(C) {
  comps = toChainComp(C)
  s = 1
  for (c in comps) {
    s = s * sizeMEC(C[as.numeric(c), as.numeric(c)])
  }
  return(s)
}


set.seed(101)

#example 1 
U = matrix(0 * c(1:25), nrow = 5, ncol = 5)
U[1,2] = 1
U[2,1] = 1
U[2,3] = 1
U[3,2] = 1
U[4,3] = 1
U[3,4] = 1
U[2,4] = 1
U[4,2] = 1
U[5,4] = 1
U[4,5] = 1
U[5,2] = 1
U[2,5] = 1
U[5,3] = 1
U[3,5] = 1
G1 = graph_from_adjacency_matrix(U)
U
SizeMECGraph(U) 

#example2
U = 0 * U
U[2,1]= 1
U[2,3] = 1
U[3,1] = 1
U[4,3] = 1
U[4,5] = 1
U[5,4] = 1
G2 = graph_from_adjacency_matrix(U)
plot(G2)
U
SizeMECGraph(U) 
