# snow code to do discrete time series prediction; data vector 
# consists of 0s and 1s; prediction at time i is whatever value, 0 or 1,
# is in the majority of the previous window of observations

# predsnow()
# arguments:
#    cls:  snow cluster
#    x:  observations vector
#    maxk:  window widths in 1:maxk will be tried; assumed to be
#           evenly divisible for the number of worker processes
# return value:  
#    length-maxk vector of fractions of mispredicted observations
predsnow <- function(cls,x,maxk) {
   nw <- length(cls)  # number of workers
   nks <- maxk / nw  # number of values of k to be done by each process
   klist <- list()
   for (i in 1:nw) {  # could do this a fancy way, say with tapply()
      start <- (i-1) * nks + 1
      end <- i * nks
      klist[[i]] <- start:end
   }
   clusterExport(cls,"pred1k")  # send pred1k function to workers
   errratesl <- clusterApply(cls,klist,dopreds,x)
   # need it all in a single vector
   errrates <- double(0)
   for (i in 1:nw) {  
      errrates <- c(errrates,errratesl[[i]])
   }
   return(errrates)
}

# find number of prediction errors in the k range krng
dopreds <- function(krng,x) {
   errrates <- vector(length=length(krng))
   startk <- krng[1]
   for (k in krng) {
      errrates[k-startk+1] <- pred1k(x,k)
   }
   return(errrates)
}

# do predictions for 1 value of k
pred1k <- function(x,k) {
   n <- length(x)
   k2 <- k / 2  # threshhold for majority vote
   # vector of predicted values for x[k+1], x[k+2], ..., x[n]
   preds <- vector(length=n-k)  
   # find predicted value for x[k+1]
   sm <- sum(x[1:k])
   if (sm >= k2) preds[1] <- 1 else preds[1] <- 0
   # now the rest
   if (n-k >= 2) {
      for (i in 2:(n-k)) {
         sm <- sm + x[i+k-1] - x[i-1]
         if (sm >= k2) preds[i] <- 1 else preds[i] <- 0
      }
   }
   return(mean(abs(preds-x[(k+1):n])))
}

# test predsnow() on a vector of length n, with k up to maxk, and with
# np worker processes
#
testsnow <- function(n,maxk,np) {
   set.seed(8888)
   x <- sample(0:1,n,replace=T)
   # set up snow cluster
   cls <- makeSOCKcluster(rep("localhost",np))
   # run the code
   print(system.time(predsnow(cls,x,maxk)))
   # shut down snow cluster
   stopCluster(cls)
}
