# continually measures access times to a given list of Web sites, to gauge
# overall Internet speed over time, while at the same time allowing the
# user to do statistical analyses on the results so far

# if the variable accesstimes is length n, then the Rdsm vector
# accesstimes stores the n most recent probed access times, with element
# i being the i-th oldest

# the code here uses wget (built in to most Unix-family systems,
# available in Cygwin for Windows) for convenience in this example, but
# other methods using R functions would be more accurate, as they would
# not include wget's startup time

# test by calling webprobe("TopSites",80) (the file TopSites is
# available at http://heather.cs.ucdavis.edu/TopSites), and then in the
# window in which the R prompt reappears, run various commands, such as
# mean(accesstimes[]) or hist(accesstimes[])

# author:  N. Matloff

# arguments:
#    sitefile: IPs, one Web site per line
#    ww: window width, desired length of accesstimes
# return value:
#    none; communicated via Rdsm variable accesstimes, which contains
#    the ww most recent access times
# locks:
#    acclock: lock guarding accesstimes
webprobe <- function(sitefile,ww) {
   cnewdsm("accesstimes","dsmv","double",rep(0,ww))
   cnewdsm("naccesstimes","dsmv","double",0)
   barr()
   # last thread is intended simply to provide access to humans, who
   # can do analyses on the data, typing commands, so have it exit this
   # function and return to the R command prompt
   if (myinfo$myid == myinfo$nclnt) {
      print("back to R now")
      return()
   } else {  # the other processes continually probe the Web:
      sites <- scan(sitefile,what="")
      nsites <- length(sites)
      repeat {
         # choose random site to probe
         site <- sites[sample(1:nsites,1)]  
         # now probe it, recording the access time
         acc <- system.time(system(paste("wget --spider -q",site)))[3]
         # add to accesstimes, in sliding-window fashion
         lock("acclock")
         if (naccesstimes[1] < ww) {
            naccesstimes[1] <- naccesstimes[1] + 1
            accesstimes[naccesstimes[1]] <- acc
         } else {
            newvec <- c(accesstimes[-1],acc)
            accesstimes[] <- newvec
         }
         unlock("acclock")
      }
   }
}
