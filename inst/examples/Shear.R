# Rdsm example: Shear.R, ShearSort

# author: N. Matloff

# disclaimer:  inefficient, used only as an illustration of Rdsm coding

# the function shear() sorts an array, stored in a square matrix; data
# is sorted in-place, with the final sorted array stored in a
# "snakelike" fashion, rowwise; e.g. see "Parallel Programming," by
# Wilkinson and Allen; this algorithm is designed for processor arrays,
# but its simplicity makes it a good example for Rdsm

# argument:
#   dm:  data matrix, an Rdsm variable; square matrix of data to be sorted
# return value:
#   none; data is sorted in-place
shear <- function(dm) {
   barr()
   nr <- dm$size[1]  # number of rows in dm
   myid <- myinfo$myid
   nclnt <- myinfo$nclnt
   numsteps <- ceiling(log2(nr*nr)) + 1
   nrc <- floor(nr/nclnt)  # number of rows/columns per client
   # determine which rows/cols this client will handle 
   mystartrc <- 1 + (myid-1) * nrc
   myendrc <- max(myinfo$myid * nrc,nr)
   for (step in 1:numsteps) {
      for (myrc in mystartrc:myendrc) {
         # even or odd step?
         if (step %% 2 == 1) {
            # even or odd row/col number?
            if (myrc %% 2 == 1) dm[myrc,] <- sort(dm[myrc,]) else
               dm[myrc,] <- sort(dm[myrc,],decreasing=T)
         } else dm[,myrc] <- sort(dm[,myrc])
      }
      barr()
   }
   return()
}

# test
testshear <- function() {
   if (myinfo$myid == 1) {
      newdsm("d","dsmm","integer",
         val=matrix(c(10:19,1,3,2,7,5,4,8,6,9,12,6,15,14,22,21,17,30:39),
            nrow=6)) 
   }  else
      newdsm("d","dsmm","integer",size=c(6,6)) 
   shear(d)
   if(myinfo$myid == 1) print(d[,])
   dsmexit()
}

# run with up to 6 clients (changing code below if not 2)

# testshear() should print out
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    1    2    3    4    5    6
# [2,]   11   10    9    8    7    6
# [3,]   12   12   13   14   14   15
# [4,]   19   18   17   17   16   15
# [5,]   21   22   30   31   32   33
# [6,]   39   38   37   36   35   34

