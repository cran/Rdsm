# continually measures access times to a given list of Web sites, to gauge
# overall Internet speed over time

# if the variable accesstimes is length n, then the Rdsm vector
# accesstimes stores the n most recent probed access times, with element
# i being the i-th oldest

# the code here uses wget (built in to most Unix-family systems,
# available in Cygwin for Windows) for convenience in this example, but
# other methods using R functions would be more accurate, as they would
# not include wget's startup time

# author:  N. Matloff

# arguments:
#    sitefile: IPs, one Web site per line
#    ww: window width, desired length of accesstimes
# return value:
#    none; communicated via Rdsm variable accesstimes
# locks:
#    acclock: lock guarding accesstimes
webprobe <- function(sitefile,ww) {
   if(myinfo$myid == 1) {
      newdsm("accesstimes","dsmv","double",val=rep(0,ww))
   } else {
      newdsm("accesstimes","dsmv","double",size=ww)
   }
   barr()
   # last process is intended simply to provide access to humans, who
   # can do analyses on the data, typing commands, so have it exit this
   # function and return to the R command prompt
   if (myinfo$myid == myinfo$nclnt) {
      return()
   } else {  # the other processes continually probe the Web:
      sites <- scan(sitefile,what="")
      nsites <- length(sites)
      repeat {
         # choose random site to probe
         site <- sites[sample(1:nsites,1)]  
         # now probe it
         acc <- system.time(system(paste("wget --spider -q",site)))[3]
         # add to accesstimes, in sliding-window fashion
         lock("acclock")
         newvec <- c(accesstimes[-1],acc)
         accesstimes[] <- newvec
         unlock("acclock")
      }
   }
}

# test by, e.g. calling webprobe("topsites",80) (the file topsites is
# included in this directory), and then in the window in which the R
# prompt reappears, run various commands, such as mean(accesstimes[]) or
# hist(accesstimes[])
