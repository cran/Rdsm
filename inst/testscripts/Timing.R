# measure timing, specifically speed at which a shared variable can be
# read

test <- function(vecsize,ntimes,rw="read") {
   me <- myinfo$myid
   if(me == 1) {
      newdsm("y","dsmv","integer",val=1:vecsize)
   } else {
      newdsm("y","dsmv","integer",size=vecsize)
   }
   message(system.time(
   for (i in 1:ntimes) {
      v <- y[]
   }
   )[[3]])
   dsmexit()
}

