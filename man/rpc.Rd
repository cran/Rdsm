\name{rpc}
\alias{rpc}
\alias{newf}
\title{RPC}

\description{Remote Procedure Call.}

\usage{
newf(ftnname,ftn)
rpc(ftnname,arglist)
}

\arguments{
\item{ftnname}{Function to be invoked remotely, quoted}
\item{ftn}{The function itself, an \R function object.}
\item{arglist}{List of arguments for the invoked function, possibly empty.}
}

\details{An \code{arglist} element is either an expression not involving
any \pkg{Rdsm} variables, in which case it is evaluated at the client side, or
a single quoted \pkg{Rdsm} variable name.  The actual execution of the
function is at the server, and may be used to affect \pkg{Rdsm} variables.
}
 
\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

