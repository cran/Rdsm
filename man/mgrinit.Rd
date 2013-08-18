\name{mgrinit}
\alias{mgrinit}

\title{
Initialize \pkg{Rdsm}
}

\description{
Initializes \pkg{Rdsm} on the given \pkg{snow} cluster.
}

\usage{
mgrinit(cls,boost=F,back=F)
}

\arguments{
  \item{cls}{
  The \pkg{snow} cluster.
  }
  \item{boost}{
  Lock functions.  If True, \code{boostlock()} and \code{boostunlock()}
  are used; otherwise \code{backlock()} and \code{backunlock()}.
  }
  \item{back}{
  If True, the count/sense variables associated with the barrier will be
  placed in backing store.
  }
}

\details{
Run this from the manager (the R process from which you create the
cluster), before creating the shared variables with \code{mgrmakevar}.
The initialization need be done only once for the life of the cluster.
For convenience, you may wish to use \code{shmcls()} to create the cluster.

If you put shared variables in backing store (\code{back = True} in
\code{mgrmakevar()}), or if you are on a Windows platform, you must 
have \code{boost = False}.  
}

% \value{
% }

\author{
Norm Matloff
}

