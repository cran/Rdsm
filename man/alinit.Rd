\name{alinit}
\alias{alinit}
\alias{cmdtoclnts}
\alias{cmdtosrvr}
\alias{cmdtoall}
\alias{go}
\alias{loadrdsm}
\title{alinit}

\description{Initiate and manage an  \pkg{Rdsm} session.}

\usage{
alinit(nclnt=NULL,nds=NULL,bigmem=F)
cmdtoclnts(cmd)
cmdtosrvr(cmd)
cmdtoall(cmd)
go(ncon=2)
loadrdsm(nclnt,bigmem=F)
}

\arguments{
\item{nclnt}{Number of clients/threads.}
\item{nds}{A character vector specifying the names of the client nodes.}
\item{bigmem}{Load \pkg{bigmemory}.}
\item{cmd}{A one-element character vector specifying a command to run on
the clients and/or server.}
\item{ncon}{Number of clients/threads.}
}

\details{These functions automate the creation and management of an
\pkg{Rdsm} session.   They must be run on a Unix-family system
(Linux, Mac OS, or Cygwin on Windows). 

Calling \code{alinit()} creates terminal windows for the server and
clients, runs \R in each of them, loads \pkg{Rdsm} and optionally
\pkg{bigmemory}.  Exactly one of \code{nclnt} and \code{nds} must be
NULL, so there are two possibilities: 

   \itemize{

      \item \code{nclnt} specifies the number of clients, all of which
         run on the local machine, i.e. \file{localhost} 

      \item \code{nds} specifies the client nodes

   }

One initializes the client/server connections by calling \code{go()}.

The core of automated \pkg{Rdsm} session lies in the use of the
functions \code{cmdtclnts()}, \code{cmdtosrvr()}, \code{cmdtoall()},
which send the given command to the clients and/or the server.  

Here is a sample session, to run a function \code{x()} contained in the
source code file \file{y.R}.:

\preformatted{
       alinit(2)  # create 2 clients
       cmdtoclnts('source("y.R")')  # have clients source the app code
       go()  # set up server/client connections
       cmdtoclnts('x(3,100)')  # first run of app
       cmdtoclnts('x(12,5000)')  # second run of app
       ...
   }

Note:  Autolaunch uses the Unix \file{screen} program.  If your
application crashes, you may need to wipe out leftover screens, both by
running \code{screen -wipe} and killing the associated processes.
}

% \seealso{
%     \code{\link{srvrloop}}
% }

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

