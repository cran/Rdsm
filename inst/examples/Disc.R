# bigmemory code to do discrete time series prediction; data vector 
# consists of 0s and 1s; prediction at time i is whatever value, 0 or 1,
# is in the majority of the previous window of observations

# predmpi()
# arguments:
#    x:  observations vector
#    maxk:  window widths in 1:maxk will be tried; assumed to be
#           evenly divisible for the number of worker processes
#    errrates:  an output, length-maxk vector of fractions of 
#               mispredicted observations 
# return value: none; global variable errrates is set by this function
predbig <- function(x,maxk,errrates) {
   if (maxk %% myinfo$nclnt != 0) 
      stop("maxk must be divisible by the number of clients")
   # determine which k range is to be done by this worker
   blksz <- maxk / myinfo$nclnt
   startk <- (myinfo$myid-1) * blksz + 1
   endk <- myinfo$myid * blksz
   xcp <- x[]
   # do the work
   for (k in startk:endk) {
      errrates[k] <- pred1k(xcp,k)
   }
   barr()
}

# do predictions on w for 1 value of k
pred1k <- function(w,k) {
   n <- length(w)
   k2 <- k / 2  # threshhold for majority vote
   # vector of predicted values for w[k+1], w[k+2], ..., w[n]
   preds <- vector(length=n-k)  
   # find predicted value for w[k+1]
   sm <- sum(w[1:k])
   if (sm >= k2) preds[1] <- 1 else preds[1] <- 0
   # now the rest
   if (n-k >= 2) {
      for (i in 2:(n-k)) {
         sm <- sm + w[i+k-1] - w[i-1]
         if (sm >= k2) preds[i] <- 1 else preds[i] <- 0
      }
   }
   return(mean(abs(preds-w[(k+1):n])))
}

testrdsm <- function(n,maxk) {
   set.seed(8888)
   if (myinfo$myid == 1) {
      newdsm("x","dsmv","integer",val=sample(0:1,n,replace=T))
      newdsm("errrates","dsmv","double",val=rep(0,maxk))
   } else {
      newdsm("x","dsmv","integer",size=n)
      newdsm("errrates","dsmv","double",size=maxk)
   }
   print(system.time(predbig(x,maxk,errrates)))
   print(errrates[])
   dsmexit()
}

# the output of testrdsm(1000,6) should be
# 0.4794795 0.5000000 0.5055165 0.5090361 0.4994975 0.4949698

