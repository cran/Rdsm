# test Remote Procedure Call

# reverse the vector with name xname in glbls, and add c to each element
rv <- function(xname,c) {
   x <- glbls[[xname]]$val 
   n <- length(x)
   glbls[[xname]]$val <<- x[n:1] + c
}

test4 <- function(c) {
   me <- myinfo$myid
   if(me == 1) {
      newdsm("y","dsmv","integer",val=1:4)
   } else {
      newdsm("y","dsmv","integer",size=4)
   }
   if(me == 1) {
      newf("rv",rv)
      rpc("rv",list("y",c))
   }
   barr()
   message("from ",me,": y = ",paste(y[],collapse=" "))
   dsmexit()
}

# run with any number of clients

# should get output of 7 6 5 4
