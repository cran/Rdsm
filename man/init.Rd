\name{init}
\alias{init}
\title{init}

\description{Client calls this to check in with the Rdsm server, which
sends back the client's ID number and the total number of clients.}

\usage{
init(host="localhost",port=2000) 
}

\arguments{
  \item{host}{Host IP address}
  \item{port}{Host TCP port number.}
}

\details{This need be run just once per call of \code{init} on the
server side.  Multiple runs of \code{serverloop} at the latter need not
be accompanied by reruns of \code{init} by the clients, and in fact
the clients should not rerun the function if they wish to retain the
current shared variables.  }

\author{Norm Matloff}

% \seealso{
%    \code{\link[init]{init}}
% }

% \examples{
% }

% \keyword{utilities}

