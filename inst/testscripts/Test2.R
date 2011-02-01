# test barrier, basic shared vector, matrix ops

# simply call test()

test <- function() {
   if (myinfo$nclnt != 3) {
      message("must be run with 3 clients")
      dsmexit()
      return()
   }
   me <- myinfo$myid
   message("I am ",me)
   cnewdsm("y","dsmv",thismode="integer",val=c(5,12,13))
   barr()
   message("should print 5 12 13")
   message(cat(y[]))  
   barr()
   y[myinfo$myid] <- 10*myinfo$myid
   barr()
   message("should print 10 20")
   message(cat(y[1:2]))  
   barr()
   if (myinfo$myid == 1) y[3] <- 8
   barr()
   message("should print 10 20 8")
   message(cat(y[]))  
   barr()
   if (myinfo$myid == 3) y[3] <- 33
   barr()
   message("should print 10 20 33")
   message(y[])  
   barr()
   cnewdsm("z","dsmv",thismode="integer",val=8)
   barr()
   z[1] <- 20
   message("should print 20")
   message(z[1])
   barr()
   cnewdsm("m","dsmm",thismode="integer",val=rbind(1:3,4:6))
   barr()
   message("should print matrix with rows 1,2,3 and 4,5,6")
   message(cat(m[1,]))  
   message(cat(m[2,]))  
   barr()
   message("should print a row 1,2,3")
   message(cat(m[1,]))  
   barr()
   message("should print 1 4")
   message(cat(m[,1]))  
   barr()
   message("should print matrix with rows 2,3 and 5,6")
   xval <- m[1:2,2:3]
   cnewdsm("x","dsmm",thismode="integer",val=xval)
   message(cat(x[1,]))  
   message(cat(x[2,]))  
   barr()
   m[1:2,2:3] <- matrix(c(8,88,888,8888),nrow=2,byrow=T)
   message("should print matrix with rows 8,88 and 888,8888")
   x[,] <- m[1:2,2:3]
   message(cat(x[1,]))  
   message(cat(x[2,]))  
}
