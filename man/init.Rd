\name{init}
\alias{init}
\title{init}

\description{Client calls this to register with the \pkg{Rdsm} server, which
sends back the client's ID number and the total number of clients.}

\usage{
init(host="localhost",port=2000) 
}

\arguments{
  \item{host}{Host IP address}
  \item{port}{Host TCP port number.}
}

\details{This need be run just once per call of \code{srvr()} on the
server side.  Clients should not rerun \code{init()} if they wish to
retain shared variables.  
}

\author{Norm Matloff}

% \seealso{
%    \code{\link[init]{init}}
% }

% \examples{
% }

% \keyword{utilities}

