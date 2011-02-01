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

\details{Call will block at the caller until all clients have made the call.  
Use to synchronize multiple threads at the same line of code.
}

\seealso{
    \code{\link{lock}},
    \code{\link{unlock}},
    \code{\link{wait}},
    \code{\link{signal}},
    \code{\link{fa}}
}

\author{Norm Matloff}

