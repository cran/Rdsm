# snow example, AllPossRegressSno.R, regresses response variable Y
# column against all possible subsets of the Xi predictor variables; 
# determines the set with the largest adjusted R2 value

# author:  N. Matloff

# arguments:
#    cl:  cluster ID
#    x:  matrix of predictors, one per column
#    y:  vector of observations on the response variable
# return value:
#    list, first element equal to max adj R2, second element the 
#    maximizing predictor set
allpossregresssnow <- function(cl,x,y) {
   p <- ncol(x)
   # generate list of predictor sets
   allcombs <- genallcombs(p)
   # parcel out the work
   results <- parLapply(cl,allcombs,regress1comb,x,y)
   # results is list of numbers, max adj R2s for each node
   # extract adj R2 values
   adjrvals <- sapply(results,function(node) return(node[[1]]))
   maxidx <- which.max(adjrvals)  # index that attained the max
   maxval <- adjrvals[maxidx]  # max value
   # which predictor set attained the max?
   maxps <- allcombs[[maxidx]]
   return(list(bestadjr2=maxval,bestps=maxps))
}

# find adj R2 for one predictor set comb
regress1comb <- function(comb,x,y) {
   slm <- summary(lm(y ~ x[,comb]))
   return(slm$adj.r.squared)
}

# generate all nonempty subsets of 1..p, up to size s; returns list,
# i-th element of which is a vector of indices from 1..p
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
# predictor set seems best, among all possible ones
testsnow <- function(n,p) {
   # generate data
   set.seed(9999)
   x <- matrix(rnorm(n*p),ncol=p)
   y <- x%*%c(1,1,1,rep(0.1,p-3)) + rnorm(n)
   # set up snow cluster
   cls <- makeSOCKcluster(c("localhost","localhost"))
   # start the work
   print(system.time(rslts <- allpossregresssnow(cls,x,y)))
   cat("max adj. R2 = ",rslts$bestadjr2,"\n")
   cat("best predictor set = ",rslts$bestps,"\n")
}

# output from testsnow(100,6) should be
# max adj. R2 =  0.8669005 
# best predictor set =  1 2 3 5 6 

