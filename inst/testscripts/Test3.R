# test locks

test <- function(n) {
   me <- myinfo$myid
   message("I am ",me)
   if (me == 1) {
      newdsm("y","dsmv","double",val=0)
   } else {
      newdsm("y","dsmv","double",size=1)
   }
   barr()
   for (i in 1:n) {
      # make arrival time to lock random, so as to test different
      # patterns of which client gets the lock first
      Sys.sleep(runif(1))  
      message(me," wants to lock")
      lock("lk")
      message(me," opens lock")
      y[1] <- y[1] + 1.2
      message(me," ready to unlock")
      unlock("lk")
   }
   barr()
   lock("msglk")
   message("from ",me,": y = ",y[1])
   unlock("msglk")
   barr()
   dsmexit()
}

# run with any number of clients

# should output 1.2 times n times number of clients
