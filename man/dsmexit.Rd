\name{dsmexit}
\alias{dsmexit}
\alias{closecon}
\title{dsmexit}

\description{Notifies the \pkg{Rdsm} server that the client
has completed the command sent to it.
}

\usage{
dsmexit()
closecon()
}

\arguments{
None.
}

\details{The server-side function \code{srvrloop()} keeps track of the 
number of clients remaining.  When that number reaches 0, that function
returns.

In other words, use of \code{dsmexit()} eventually results in a shutdown
of the server, which then must be restarted if you wish to run further
\pkg{Rdsm} applications in this session.  Thus \code{dsmexit()} should
\emph{not} be used in most applications.
}

\seealso{
    \code{\link{srvr}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

