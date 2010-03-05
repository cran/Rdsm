\name{srvr}
\alias{srvr}
\alias{srvrinit}
\alias{srvrloop}
\title{srvrinit}

\description{Server-side code.  Invocation of the \pkg{Rdsm} server.}  

\usage{
srvrinit(port=2000,ncon=2) 
srvrloop() 
srvr(port=2000,ncon=2) 
}

\arguments{
  \item{port}{Server TCP port number}
  \item{ncon}{Number of connections, i.e. number of clients.}
}

\details{The function \code{srvrinit} accepts client requests for
connection with the server, and returns to the the client its ID and the
total number of clients.  Then \code{srvrloop} is called to run the
server.

Then, at each client, \code{init} is called, then the application function.

One may use \code{srvr} in lieu of the pair \code{srvrinit} and
\code{srvrloop}.

You may have several application functions to run, or may want to run
the same one multiple times.  This is fine as long as you don't rerun
\code{srvrinit()}; you must rerun \code{srvrloop}, but do not rerun
\code{init} at the clinets.
Application-program \pkg{Rdsm} variables etc. will be retained from one
run to the next.

}

\author{Norm Matloff}

\seealso{
    \code{\link{init}}
}

% \examples{
% }

% \keyword{utilities}

