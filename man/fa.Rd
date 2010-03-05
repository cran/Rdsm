\name{fa}
\alias{fa}
\title{fa}

\description{Fetch-and-add function.}

\usage{
fa(fav,inc)
}

\arguments{
\item{fav}{Fetch-and-add variable (integer vector of length 1), quoted.}
\item{inc}{Increment value.}
}

\details{When a client calls \code{fa} on \code{fav}, the quantity
\code{inc} (which could be negative) will be added to \code{fa},
atomically.}

\seealso{
     \code{\link{barr}},
     \code{\link{wait}},
     \code{\link{signal}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

