
# inputs a graph adjacency matrix, and outputs a two-column matrix
# listing the edges emanating from each node

# e.g. 

#       [,1] [,2] [,3] [,4] [,5] [,6]
#  [1,]    1    0    0    1    1    0
#  [2,]    1    1    0    1    0    1
#  [3,]    1    0    1    0    0    1
#  [4,]    1    1    1    0    1    0
#  [5,]    1    1    0    1    0    1
#  [6,]    0    1    0    0    0    1
#  
#  produces
#  
#        [,1] [,2]
#   [1,]    1    1
#   [2,]    1    4
#   [3,]    1    5
#   [4,]    2    1
#   [5,]    2    2
#   [6,]    2    4
#   [7,]    2    6
#   [8,]    3    1
#   [9,]    3    3
#  [10,]    3    6
#  
#  etc.

# arguments:
#    a:  adjacency matrix 
#    lnks:  edges matrix; shared, nrow(a)^2 rows and 2 columns
#    counts:  numbers of edges found by each thread; shared

# in this version, the matrix lnks must be created ahead of time; since
# the number of rows is uknown a priori, one must allow for the worst
# case, nrow(a)^2 rows; after the run, the number of actual rows will be
# in counts[1,length(cls)]

getlinks <- function(a,lnks,counts) {
   require(parallel)
   nr <- nrow(a)
   # get my assigned portion of a
   myidxs <- getidxs(nr)
   myout <- apply(a[myidxs,],1,function(rw) which(rw==1))
   # myout[[i]] now lists the edges from node myidxs[1] + i - 1
   nmyedges <- Reduce(sum,lapply(myout,length))  # my total edges
   me <- myinfo$id
   counts[1,me] <- nmyedges
   barr()
   if (me == 1) {
      # use cumsum() to determine where each node will store its results
      # in lnks
      counts[1,] <- cumsum(counts[1,])
   }
   barr()
   # lnksidx will be the next row to write within lnks
   lnksidx <- if (me == 1) 1 else counts[1,me-1] + 1
   for (idx in myidxs)  {
      # corresponding index to idx within myout
      jdx <- idx - myidxs[1] + 1
      myoj <- myout[[jdx]]
      if (!is.null(myoj)) {
         end <- lnksidx + length(myoj) - 1
         lnks[lnksidx:end,] <- cbind(idx,myoj)
         lnksidx <- end + 1
      }
   }
   invisible(0)  # don't do expensive return of result
}

test <- function(cls) {
   require(parallel)
   mgrinit(cls)
   mgrmakevar(cls,"x",8,8)
   mgrmakevar(cls,"lnks",64,2)
   mgrmakevar(cls,"counts",1,length(cls))
   x[,] <- matrix(sample(0:1,64,replace=T),ncol=8)
   print(x[,])
   clusterExport(cls,"getlinks")
   clusterEvalQ(cls,getlinks(x,lnks,counts))
   nout <- counts[1,length(cls)]
   print(lnks[1:nout,])
}
