\name{dsmv-class}
\alias{dsmv-class}
\alias{[.dsmv}
\alias{[.dsmm}
\alias{[.dsml}
\alias{[<-.dsmv}
\alias{[<-.dsmm}
\alias{[<-.dsml}
\title{Rdsm Classes}

\description{The \code{dsmv}, \code{dsmm} and \code{dsml} classes act
like ordinary \R vectors, matrices and lists, but have internal
operations allowing them to be shared across \R processes.}

\details{These classes are created by calling \code{cnewdsm()}.  Indexing
operations with \code{[]} work syntactically as with ordinary \R
vectors, matrices and lists, but the generic \R functions \code{[} and
\code{[<-} are implemented in special versions to enable the sharing.

(Currently the list functions \code{[[ ]]} and \code{$} are not yet
implemented, nor is increasing the number of components of a shared list.)

Note carefully that you must always use brackets with \pkg{Rdsm}
variables.  For instance, to copy the \pkg{Rdsm} vector \code{x} to an
ordinary \R variable \code{y}, write

\preformatted{ y <- x[] }

not

\preformatted{ y <- x }

}

\seealso{
     \code{\link{cnewdsm}}
}

\author{Norm Matloff}

% \examples{
% }

% \keyword{utilities}

