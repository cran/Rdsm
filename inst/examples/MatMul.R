# Rdsm tutorial: MatMult.R, simple matrix multiplication code

# authors: Y. Hui, N. Matloff

# ********************************************************************
# ********************************************************************
# ********************************************************************
# ********************************************************************
# ********************      READ THIS FIRST     **********************
#
# the purpose of this program is educate the Rdsm novice on the basic
# writing and execution of Rdsm code; it has extensive comments designed
# with that goal in mind; there is also additional output for that
# purpose
#
# ********************************************************************

# ********************************************************************
# ********************************************************************
# ********************      TEST RUN     *****************************
#
# instructions for starting Rdsm are given in the README
# file in this directory
#
# run test("r") (Rdsm matrix) or test("b") (bigmemory matrix) with 2 or
# 3 clients
#
# should print out 
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]  441 1017 1593 2169 2745 3321
# [2,]  462 1074 1686 2298 2910 3522
# [3,]  483 1131 1779 2427 3075 3723
# [4,]  504 1188 1872 2556 3240 3924
# [5,]  525 1245 1965 2685 3405 4125
# [6,]  546 1302 2058 2814 3570 4326
# 
# ********************************************************************
# ********************************************************************

# the function matmul() takes in two matrices and computes their product
#
# arguments, all Rdsm or bigmemory variables:
#   m1:  left matrix, 
#   m2:  right matrix
#   prd:  product
#
# return value:  none; product is stored in prd

# for simplicity, assume here that the number of columns in m1 is a
# multiple of the number of clients
matmul <- function(m1, m2, prd) {
   # myinfo is a variable built-in to Rdsm; it includes the client's ID
   myid <- myinfo$myid
   # we need to know the number of columns of m1; easier to query for
   # bigmemory case
   k <- if(class(m1) == "big.matrix") dim(m1)[2] else m1$size[2]  
   # determine how many columns of prd will be done by each client; one
   # of the components of myinfo is nclnt, the total number of clients
   chunksize <- k/myinfo$nclnt
   firstcol <- 1 + (myid-1) * chunksize
   lastcol <- firstcol + chunksize - 1
   # process this client's share of the columns: multiply them times m2,
   # and place the result in the proper part of prd
   prd[,firstcol:lastcol] <- m1[,] %*% m2[,firstcol:lastcol]
   # barr(), a barrier function, a workhorse of shared-memory
   # programming, ensures that the clients are "all on the same page,"
   # meaning on the same line of code; when this function is called by a
   # client; it hangs until all other clients call the function, so that
   # we won't leave until the entire prd computation is done
   barr()
}

# function to test matmul()
test <- function(smtype) {
   # first create the matrices, of Rdsm or bigmemory type, as requested via
   # smtype ("r" or "b")
   m <- matrix(1:36,nrow=6)
   if (smtype == "r") {
      # create an Rdsm matrix (class "dsmm") named "ma", double mode, 
      # initial value m
      cnewdsm("ma",thisclass="dsmm",thismode="double",val=m)
      cnewdsm("mb","dsmm","double",m)
      cnewdsm("mc","dsmm","double",val=matrix(rep(0,36),nrow=6))
   } else {
       # create a bigmemory matrix "ma", double mode, 6x6, initial
       # value m
       newbm("ma","double",6,6,m)
       newbm("mb","double",6,6,m)
       newbm("mc","double",6,6,m)
   }
   # call the multiplication code; note again that all the arguments are
   # global, and mc here will be changed
   matmul(ma,mb,mc)
   # print result if we are thread 1
   if (myinfo$myid == 1) print(mc[,])
}
