# Rdsm example, AllPossRegress.j, regresses response variable Y column
# against all possible subsets of the Xi predictor variables; returns
# the one with the largest adjusted R2 value

# author:  N. Matloff

# arguments:
#    x:  matrix of predictors, one per column
#    y:  vector of observations on the response variable
#    maxar2:  largest adjusted R2 so far; an Rdsm vector of length 1; 
#       must be initialized to NA before call
#    maxar2idx:  indices of the predictor set with the largest adjusted
#       R2 so far; an Rdsm vector
# return value:
#    none; communicated via maxar2, maxar2idx
# locks:
#    mxlock:  locks access to maxar2, maxar2idx
allpossregress <- function(x,y,maxar2,maxar2idx) {
   p <- ncol(x)
   # generate list of subsets
   allcombs <- genallcombs(p)
   ncombs <- length(allcombs)
   # determine which combs this process will handle
   myid <- myinfo$myid
   nclnt <- myinfo$nclnt
   batchsize <- floor(ncombs/nclnt)
   bgn <- (myid-1) * batchsize + 1
   fin <- if (myid == nclnt) ncombs else myid*batchsize
   mymax <- NA  # max found among all my batches
   mymaxidx <- NA
   # find the adjusted R-squared values for predictor set ps
   do1predset <- function(ps) {
      slm <- summary(lm(y ~ x[,ps]))
      return(slm$adj.r.squared)
   }
   # regress on all the combs in my batch
   adjr2 <- sapply(allcombs[bgn:fin],do1predset)
   # which one was best, and what was its adj R^2 value?
   mymaxidx <- which.max(adjr2)
   mymax <- adjr2[mymaxidx]
   # update global values if mine is better
   lock("mxlock")
   if (mymax > maxar2[1]) {
      maxar2[1] <- mymax
      # find predictor set
      ps <- allcombs[bgn+mymaxidx-1]
      # fill in 0s in maxar2idx so previous values are not retained
      towrite <- c(ps[[1]],rep(0,p-length(ps[[1]])))
      maxar2idx[] <- towrite
   }
   unlock("mxlock")
   barr()
}

# generate all nonempty subsets of 1..p; returns list, i-th element of
# which is a vector of indices from 1..p
genallcombs <- function(p) {
   allcombs <- list()
   for (i in 1:p) {
      allcombs <- c(allcombs,matcols2lst(combn(1:p,i)))
   }
   return(allcombs)
}

# for matrix m, returns a list consisting of m's cols 
matcols2lst <- function(m) {
   lst <- list()
   # add a row of col numbers to m
   bigm <- rbind(1:ncol(m),m) 
   tolst <- function(col) lst[[col[1]]] <<- col[-1]
   apply(bigm,2,tolst)
   return(lst)
}

# simulate n observations with p predictor variables, and see what
# predictor set seems best
test <- function(n,p) {
   set.seed(9999)
   x <- matrix(rnorm(n*p),ncol=p)
   y <- x%*%c(1,1,1,rep(0.1,p-3)) + rnorm(n)
   if(myinfo$myid == 1) {
      newdsm("cm","dsmv","double",val=-1)
      newdsm("cmidx","dsmv","integer",val=rep(0,p))
   } else {
      newdsm("cm","dsmv","double",size=1)
      newdsm("cmidx","dsmv","integer",size=p)
   }
   barr()
   print(system.time(allpossregress(x,y,cm,cmidx)))
   barr()
   if(myinfo$myid == 1) {
      cat("max adj. R2 = ",cm[1],"\n")
      idx <- cmidx[]
      idx <- idx[idx > 0]
      cat("best predictor set = ",idx,"\n")
   }
   dsmexit()
}

# output from test(100,6) should be
# max adj. R2 =  0.8669005 
# best predictor set =  1 2 3 5 6 

