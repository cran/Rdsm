# test fetch-and-add

test <- function(n) {
   me <- myinfo$myid
   if(me == 1) {
      newdsm("y","dsmv","integer",val=0)
   } else {
      newdsm("y","dsmv","integer",size=1)
   }
   barr()
   for (i in 1:n) {
      message(me,":  y = ",y[])
      fa("y",i)
   }
   message(me," exits loop")
   barr()
   lock("msglk")
   message("from ",me,": y = ",y[])
   unlock("msglk")
   dsmexit()
}

# run with any number of clients

# should get output of 15 times number of clients
