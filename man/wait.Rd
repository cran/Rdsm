\name{wait}
\alias{wait}
\alias{signal}
\title{wait}

\description{Threads-like wait, signal}

\usage{
wait(waitvarname)
signal(waitvarname)
}

\arguments{
\item{waitvarname}{Wait variable, quoted.}
}

\details{When a client calls \code{wait} on \code{waitvarname}, \pkg{Rdsm}
will add this client's ID to a list.  The call will block.  When another
client later calls \code{signal}, \pkg{Rdsm} will then release all the
clients, i.e. their calls to \code{wait} will unblock.

One does not use \code{newdsm} to create a wait variable.  Instead, the
variable is automatically created the first time \code{wait} is called
on it.

A signal posted when no waits are pending will be ignored.

}

\seealso{
     \code{\link{barr}},
     \code{\link{lock}},
     \code{\link{unlock}},
     \code{\link{fa}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

