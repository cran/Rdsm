\name{rdsmlock, rdsmunlock}
\alias{rdsmlock}
\alias{rdsmunlock}

\title{
Lock/unlock operations.
}

\description{
Lock/unlock operations to avoid race conditions among the threads.  
}

\usage{
rdsmlock(lck)
rdsmunlock(lck)
}

\arguments{
  \item{lck}{
  Lock name.  Quoted if \code{boost = False} in \code{mgrinit()}, else
  not quoted.
  }
}

\details{
Standard lock/unlock operations from the threaded coding world.  When
one thread executes \code{rdsmlock()}, any other thread attempting to do so
will block until the first thread executes \code{rdsmunlock()}.  If a thread
does \code{rdsmlock()} on an unlocked lock, the thread call immediately
returns and the thread continues.

These functions are set at runtime, to either \code{boostlock} and
\code{boostunlock()} or \code{backlock} and \code{backunlock()},
depending on whether you set \code{boost} to True or False,
respectively, in your call to \code{mgrinit()}.  NOTE CAREFULLY:  The
functions are in turn assigned to \code{realrdsmlock} and
\code{realrdsmunlock} in the threads at run time, and these latter two
are the ones that application code should call.
}

% \value{
% }

\author{
Norm Matloff
}

\examples{
library(parallel)
c2 <- shmcls(2)  # form 2-thread Snow cluster
mgrinit(c2)  # initialize Rdsm

mgrmakevar(c2,"tot",1,1)

# this function is not reliable; if 2 threads both try to increment the
# total at about the same time, they could interfere with each other
s <- function(n) {
   for (i in 1:n) {
      tot[1,1] <- tot[1,1] + 1
   }
}

clusterExport(c2,"s")
tot[1,1] <- 0
clusterEvalQ(c2,s(1000))
tot[1,1]  # should be 2000, but likely far from it

# here is the reliable version, surrounding the increment by lock and
# unlock, so only 1 thread can execute it at once
s1 <- function(n) {
   for (i in 1:n) {
      realrdsmlock("totlock")
      tot[1,1] <- tot[1,1] + 1
      realrdsmunlock("totlock")
   }
}

mgrmakelock(c2,"totlock")

tot[1,1] <- 0
clusterExport(c2,"s1")
clusterEvalQ(c2,s1(1000))
tot[1,1]  # will print out 2000, the correct number
}


