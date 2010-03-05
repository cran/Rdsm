\name{barr}
\alias{barr}
\title{barr}

\description{Classical shared-memory barrier function.  
}

\usage{
barr()
}

\arguments{
None.
}

\details{Synchronization function.  Call will not return until all
clients have made the call.  }

\seealso{
    \code{\link{lock}},
    \code{\link{unlock}},
    \code{\link{wait}},
    \code{\link{signal}},
    \code{\link{fa}}
}

\author{Norm Matloff}

