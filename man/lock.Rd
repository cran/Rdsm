\name{lock}
\alias{lock}
\alias{unlock}
\title{lock}

\description{Classical lock, unlock functions.}

\usage{
lock(lockname)
unlock(lockname)
}

\arguments{
\item{lockname}{Lock variable, quoted.}
}

\details{When a client calls \code{lock()} on \code{lockname}, \pkg{Rdsm} will
check whether \code{lockname} is locked.  If the lock is unlocked,
\pkg{Rdsm} will lock it, and \code{lock()} will immediately return.  If on
the other hand the variable is locked, this client will join the queue
for the lock, and its call to \code{lock()} will block.

When a client calls \code{unlock()}, the call will immediately return.
\pkg{Rdsm} will then remove that client from the queue, and will check
whether the queue for the lock is still nonempty.  If so, then the call
to \code{lock()} made previously by the new head of the queue will now
return.  

One does not use \code{newdsm()} to create a lock variable.  Instead, the
variable is automatically created the first time \code{lock()} is called
on it.

}

\seealso{
     \code{\link{barr}},
     \code{\link{wait}},
     \code{\link{signal}},
     \code{\link{fa}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

