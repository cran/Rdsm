
# matrix multiplication; the product u %*% v is computed on cls, and
# stored in w; u, v and w are big.matrix objects

# the result will be in w, rather than being returned by the call

mmul <- function(u,v,w) {
   require(parallel)
   myidxs <- splitIndices(nrow(u),myinfo$nwrkrs)[[myinfo$id]]
   w[myidxs,] <- u[myidxs,] %*% v[,]
   invisible(0)  # don't do expensive return of result
}

test <- function(cls) {
   require(parallel)
   mgrinit(cls)
   mgrmakevar(cls,"a",6,2)
   mgrmakevar(cls,"b",2,6)
   mgrmakevar(cls,"c",6,6)
   a[,] <- 1:12
   b[,] <- 1  # all 1s
   clusterExport(cls,"mmul")
   clusterEvalQ(cls,mmul(a,b,c))
   print(c[,])
}

