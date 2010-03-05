# KNN.R:  k-nearest neighbor regression, sequential version

# author:  N. Matloff

# disclaimer:  code is intended purely as an example and is definitely
# not optimized

# arguments:
#   x:  vector of predictor values (not an Rdsm object)
#   y:  vector of response values (not an Rdsm object)
#   k:  number of neareast neighbors to be used
# return value:  cross-validated mean-squared error
knn <- function(x,y,k) {
   dist <- function(x) return((xi-x)^2)  # don't bother with sqrt
   n <- length(x)
   sumse <- 0  # this client's current sum of squared prediction errors
   for (i in 1:n) {
      xi <- x[i]
      dists <- dist(x)
      # get the k nearest x[j] to this x[i]
      sdists <- sort(dists,partial=2:(k+1))  # don't count x[i]
      nrstidxs <- match(sdists[2:(k+1)],dists)  # indexes of nearest x[j]
      predictedy <- mean(y[nrstidxs])
      sumse <- sumse + (predictedy-y[i])^2
   }
   return(sumse)
}

testseq <- function(n,k) {
   x <- rnorm(n)
   y <- x + rnorm(n)
   print(knn(x,y,k))
}

