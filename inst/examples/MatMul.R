# Rdsm tutorial: MatMult1.R, matrix multiplication code

# authors: Y. Hui, N. Matloff

# ********************************************************************
#
#                           READ THIS FIRST
#
# the purpose of this program is educate the Rdsm novice on the basic
# writing and execution of Rdsm code; it has extensive comments designed
# with that goal in mind; there is also additional output for that
# purpose; instructions for running the code are given in the README
# file in this directory
#
# ********************************************************************

# the function matmult1() takes in two matrices and computes their product
#
# arguments:
#   m1:  left matrix, an Rdsm matrix variable
#   m2:  right matrix, an Rdsm matrix variable
#   prd:  product, an Rdsm matrix variable
#
# return value:  none; product is stored in prd 

# for simplicity, assume here that the number of columns in m1 is a
# multiple of the number of clients
matmul1 <- function(m1, m2, prd) {
   # myinfo is a variable built-in to Rdsm; it includes the client's ID
   myid <- myinfo$myid
   # barr(), a barrier function, a workhorse of shared-memory
   # programming, ensures that the clients are "all on the same page,"
   # meaning on the same line of code; when this function is called by a
   # client; it hangs until all other clients call the function
   message(myid," reaches the barrier")
   barr()
   # m1, m2 and prd are Rsdm matrices, i.e. of class dsmm; one member
   # variable in that class is size, a 2-tuple giving the dimensions of
   # the matrix
   k <- m1$size[2]  # number of columns of m1
   # determine how many columns of m1 will be done by each client; one
   # of the components of myinfo is nclnt, the total number of clients
   chunksize <- k/myinfo$nclnt
   # determine which columns this client will process
   firstcol <- 1 + (myid-1) * chunksize
   lastcol <- firstcol + chunksize - 1
   message("\n",myid," will handle columns ",
      paste(firstcol:lastcol,collapse=" "))
   # process this client's share of the columns
   prd[,firstcol:lastcol] <- m1[,] %*% m2[,firstcol:lastcol]
   barr()
}

# function to test matmul1
test1 <- function() {
# first create the matrices 
# we use newdsm() to create Rdsm variables; here we create one named
# ma, of mode integer, with initial value as given, and then create mb
# and mc; one client, say client 1, specifies the value, which goes to
# the server, while the other clients keep only local information, i.e.
# the size; the latter is a 2-tuple for a matrix, and a scalar for a
# vector
   m <- matrix(1:36,nrow=6)
   if (myinfo$myid == 1) {
      newdsm("ma","dsmm","integer",val=m)
      newdsm("mb","dsmm","integer",val=m)
      newdsm("mc","dsmm","integer",val=matrix(nrow=6,ncol=6))
   } else {
      newdsm("ma","dsmm","integer",size=c(6,6))
      newdsm("mb","dsmm","integer",size=c(6,6))
      newdsm("mc","dsmm","integer",size=c(6,6))
   }
   # call the multiplication code; note again that all Rdsm variables are
   # global, and mc here will be changed
   matmul1(ma,mb,mc)
   # print result 
   if (myinfo$myid == 1) print(mc[,])
   # dsmexit() is used to disconnect the clients from the server
   dsmexit()
}

# to be run after test1()
test2 <- function() {
# the size; the latter is a 2-tuple for a matrix, and a scalar for a
# vector
   matmul1(ma,mc,mb)
   # print result 
   if (myinfo$myid == 1) print(mb[,])
   # dsmexit() is used to disconnect the clients from the server
   dsmexit()
}

# run with 2 or 3 clients 

# test1() should print out 
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]  441 1017 1593 2169 2745 3321
# [2,]  462 1074 1686 2298 2910 3522
# [3,]  483 1131 1779 2427 3075 3723
# [4,]  504 1188 1872 2556 3240 3924
# [5,]  525 1245 1965 2685 3405 4125
# [6,]  546 1302 2058 2814 3570 4326

# test2() should print out 
#       [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
# [1,] 49581 117297 185013 252729 320445 388161
# [2,] 52542 124254 195966 267678 339390 411102
# [3,] 55503 131211 206919 282627 358335 434043
# [4,] 58464 138168 217872 297576 377280 456984
# [5,] 61425 145125 228825 312525 396225 479925
# [6,] 64386 152082 239778 327474 415170 502866

