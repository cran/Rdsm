\name{cnewdsm}
\alias{cnewdsm}
\alias{newdsm}
\alias{newbm}
\title{cnewdsm}

\description{Create a new \pkg{Rdsm} or \pkg{bigmemory} variable.}

\usage{
cnewdsm(varname,thisclass,thismode=NULL,val) 
newdsm(varname,thisclass,thismode=NULL,val=NULL,size=NULL) 
newbm(varname,thismode,nr,nc,val=NULL) 
}

\arguments{
\item{varname}{Name of variable to be created.}
\item{thisclass}{Class of \pkg{Rdsm} variable:  "dsmv", "dsmm" or "dsml".}
\item{thismode}{\R mode of variable:  NULL for the "dsml" \pkg{Rdsm}
   case,  "integer" or "double" for the other cases; "double" is
   recommended for \pkg{bigmemory}.}
\item{val}{Initial value of variable.}
\item{size}{Size of \pkg{Rdsm} variable:  length for a vector, row/col
dimension for a matrix.}
\item{nr}{Number of rows of \pkg{bigmemory} variable.}
\item{nc}{Number of columns of \pkg{bigmemory} variable}
}

\details{The functions \code{cnewdsm()} and \code{newdsm()} create new
\pkg{Rdsm} variables, while \code{newbm()} creates new \pkg{bigmemory}
variables.

For \pkg{Rdsm} variables, one ordinarily calls \code{cnewdsm()}, resorting
to \code{newdsm()} only if finer control is needed.

In the case of \code{cnewdsm()}, all clients execute the same call.  But
if the initial value of the variable is to be that of a non-shared
variable in some thread, then that thread calls the function
\code{newdsm()} with a non-NULL \code{val} but a NULL \code{size}, while
the other clients specify NULL for \code{val} but a non-NULL for
\code{size}.  

For \pkg{bigmemory} variables, \code{val} is not very useful; it's
easier to leave it at NULL and then set the variable after the call.

}

% \seealso{
%     \code{\link{srvrloop}}
% }

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

