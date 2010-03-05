\name{newdsm}
\alias{newdsm}
\title{newdsm}

\description{Create a new \pkg{Rdsm} variable.}

\usage{
newdsm(varname,thisclass,thismode=NULL,val=NULL,size=NULL) 
}

\arguments{
\item{varname}{Name of \pkg{Rdsm} variable to be created.}
\item{thisclass}{Class of \pkg{Rdsm} variable:  "dsmv", "dsmm" or "dsml".}
\item{thismode}{\R mode of \pkg{Rdsm} variable:  NULL for the "dsml"
   case,  "integer" or "double" for the other two cases.}
\item{val}{Initial value of \pkg{Rdsm} variable.}
\item{size}{Size of \pkg{Rdsm} variable:  length for a vector, row/col
dimension for a matrix.}
}

\details{This creates a new \pkg{Rdsm} variable.  It must be executed by all
clients, but in different forms:  One client, say client 1, calls the 
function with a non-NULL \code{val} but a NULL \code{size}, while the
other clients specify NULL for \code{val} but a non-NULL for \code{size}.
}

% \seealso{
%     \code{\link{srvrloop}}
% }

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

