# Rdsm example, KNN.R:  k-nearest neighbor regression, single predictor;
# assumes all X values are distinct

# run with any number of clients
# test() should print out 104

# author:  N. Matloff

# arguments:
#   x:  vector of predictor values; not an Rdsm object
#   y:  vector of response values; not an Rdsm object
#   k:  number of nearest neighbors to be used; not an Rdsm object
#   tspe:  cross-validated total squared prediction error; Rdsm vector
#      of length 1
#   no return value; output stored in tspe
knn <- function(x,y,k,tspe) {
   # distance function; don't bother with sqrt
   dist <- function(x) return((xi-x)^2)  
   barr()
   n <- length(y)
   mysumse <- 0  # this thread's current sum of squared prediction errors
   myid <- myinfo$myid
   # how many observations will this thread process?
   chunksize <- floor(n/myinfo$nclnt)
   # which observations?
   startchunk <- 1 + (myid-1) * chunksize 
   endchunk <- if (myid < myinfo$nclnt) myid*chunksize else n
   # now process the chunk
   for (i in startchunk:endchunk) {
      xi <- x[i]
      # find distances of this xi to all of them
      dists <- dist(x)
      # get the k nearest x[j] to this x[i], excluding this one
      # disclaimer:  may not be the fastest way
      sdists <- sort(dists,partial=2:(k+1))  # don't count x[i]
      # get indexes of k nearest x[j] 
      nrstidxs <- match(sdists[2:(k+1)],dists)  
      predictedy <- mean(y[nrstidxs])
      mysumse <- mysumse + (predictedy-y[i])^2
   }
   fa("tspe",mysumse)
   barr()
}

test <- function() {
   me <- myinfo$myid
   # total squared prediction error
   cnewdsm("tspe","dsmv","double",0)
   barr()
   # our (toy) sample data
   # cnewdsm("smpl","dsmm","double",
   #   val=rbind(c(1,10),c(1.2,6),c(2.4,8),c(2.6,2)))
   smpl <- rbind(c(1,10),c(1.2,6),c(2.4,8),c(2.6,2))
   knn(smpl[,1],smpl[,2],1,tspe)
   # should print out 104
   if(me == 1) message("tspe = ",tspe[1])
}

