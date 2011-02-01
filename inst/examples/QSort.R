# Rdsm example, QSort.R, multiclient, 1-level quicksort:  for nclnt
# clients, nclnt-1 pivot elements will be selected from the vector to be
# sorted; then each client i forms its pile, consisting of the elements
# between pivots i-1 and i, then sorts its pile, then returning result
# to its proper place in the original vector; not claimed to be
# efficient

# test() should print out 5 12 13 21 22 23

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

# test 
test <- function() {
   cnewdsm("z","dsmv","double",c(12,5,13,23,22,21))
   cnewdsm("pl","dsmv","integer",rep(0,6))
   qsrt(z,pl)
   if (myinfo$myid == 1) print(z[])
}

