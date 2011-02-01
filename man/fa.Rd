\name{fa}
\alias{fa}
\title{fa}

\description{Fetch-and-add function.}

\usage{
fa(fav,inc)
}

\arguments{
\item{fav}{Fetch-and-add variable (\pkg{Rdsm} integer vector of 
length 1), name quoted.} 
\item{inc}{Increment value.}
}

\details{When a client calls \code{fa()} on \code{fav}, the quantity
\code{inc} (which could be negative) will be added to \code{fav},
atomically, i.e. without interference by another thread.  The actual
addition will be performed at the server, thus reducing communications
costs.
}

\seealso{
     \code{\link{barr}},
     \code{\link{wait}},
     \code{\link{signal}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

