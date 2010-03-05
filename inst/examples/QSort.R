# Rdsm example, QSort.R, multiclient, 1-level quicksort:  for nclnt
# clients, nclnt-1 pivot elements will be selected from the vector to be
# sorted; then each client i forms its pile, consisting of the elements
# between pivots i-1 and i, then sorts its pile, then returning result
# to its proper place in the original vector

# author:  N. Matloff

# arguments:
#    x:  vector to be sorted, an Rdsm object
#    pilelengths:  i-th one is the length of the pile to be sorted by 
#       client i, calculated and written from within the function
# return value:
#    none; vector is sorted in-place
qsrt <- function(x,pilelengths) {
   barr()
   myid <- myinfo$myid
   nclnt <- myinfo$nclnt
   n <- x$size
   if (n < 3*nclnt) {
      if (myinfo$myid == 1) {
         x[] <- sort(x[])
      }
      return()
   }
   # we'll be doing a lot of work on x, so make a local copy
   xcpy <- x[]  
   # take every third observation for our pivots
   pvts <- xcpy[3*(1:(nclnt-1))]
   pvts <- sort(pvts)
   # now client will get its pile and post the pile's length
   if (myid == 1) {
      y <- xcpy[xcpy <= pvts[1]]
   } else if (myid <= nclnt-1) {
      y <- xcpy[xcpy <= pvts[myid] & xcpy > pvts[myid-1]]
   } else {
      y <- xcpy[xcpy > pvts[nclnt-1]]
   }
   pilelengths[myid] <- length(y)
   # now use pilelengths to determine where to put this client's sorted
   # pile
   barr()  # make sure pilelengths is ready
   if (length(y) > 0) {
      start <- 1
      if (myid > 1) 
         for (i in 1:(myid-1)) start <- start + pilelengths[i]
      x[start:(start+length(y)-1)] <- sort(y) 
   }
   barr()
}

# test 1
testqsrt1 <- function() {
   if (myinfo$myid == 1) {
      newdsm("z","dsmv","integer",val=c(12,5,13,23,22,21))
   } else newdsm("z","dsmv","integer",size=6)
   if (myinfo$myid == 1) {
      newdsm("pl","dsmv","integer",val=rep(0,6))
   } else newdsm("pl","dsmv","integer",size=6)
   qsrt(z,pl)
   if (myinfo$myid == 1) print(z[])
   dsmexit()
}

# test 2
testqsrt2 <- function(n) {
   if (myinfo$myid == 1) {
      newdsm("z","dsmv","integer",val=sample(1:1000000,n,replace=T))
   } else newdsm("z","dsmv","integer",size=n)
   nc <- myinfo$nclnt
   if (myinfo$myid == 1) {
      newdsm("pl","dsmv","integer",val=rep(0,nc))
   } else newdsm("pl","dsmv","integer",size=nc)
   # vector before sorting
   if (n < 100 && myinfo$myid == 1) print(z[])
   qsrt(z,pl)
   # vector after sorting
   if (n < 100 && myinfo$myid == 1) print(z[])
   dsmexit()
}

# testqsrt1() should print out [1]  5 12 13 21 22 23

