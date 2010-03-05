# Rdsm test, Test7.R: test dsml

test <- function() {
   me <- myinfo$myid
   if(me == 1) {
      newdsm("y","dsml",val=list(x=c(5,12,13),y="abc"))
   } else
      newdsm("y","dsml",size=2)
   barr()
   print("should print 5 12 13, abc")
   print(y[])
   barr()
   y[] <- list(x=c(3,4,5),y="de")
   barr()
   print("should print 5 12 13, abc")
   print(y[])
   barr()
   y[2] <- 88
   print("should print 5 12 13, 88")
   print(y[])
   barr()
   print("should print 88")
   print(y[2])
   barr()
   dsmexit()
}
