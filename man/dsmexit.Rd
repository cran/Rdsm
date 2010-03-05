\name{dsmexit}
\alias{dsmexit}
\title{dsmexit}

\description{Notifies the Rdsm server that the client
has completed the command sent to it.
}

\usage{
dsmexit()
}

\arguments{
None.
}

\details{The server-side function \code{srvrloop} keeps track of the 
number of clients remaining.  When that number reaches 0, the function
returns.
}

\seealso{
    \code{\link{srvrloop}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

