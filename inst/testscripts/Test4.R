# test fetch-and-add

# run with any number of clients

# should get output of number of clients times n(n+1)/2

test <- function(n) {
   me <- myinfo$myid
   cnewdsm("y","dsmv","integer",0)
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
}
