\name{mgrmakevar}
\alias{mgrmakevar}

\title{
Create an \pkg{Rdsm} shared variable.
}

\description{
Creates an \pkg{Rdsm} shared variable. 
}

\usage{
mgrmakevar(cls,varname,nr,nc,vartype="double",back=F,mgrcpy=T)
}

\arguments{
  \item{cls}{
  The \pkg{snow} cluster.
  }
  \item{varname}{
  Name of the shared variable, quoted.  (The variable must
  be a matrix, though it could be 1x1 etc.)
  }
  \item{nr}{
  Number of rows in the variable.
  }
  \item{nc}{
  Number of columns in the variable.
  }
  \item{vartype}{
  Atomic R type of the variable, quoted, "double" by default.
  }
  \item{back}{
  Place in backing store?  False by default.
  }
  \item{mgrcpy}{
  Place a copy of the shared variable on the manager node?  True by default.
  }
}

\details{
Run this from the manager (the R process from which you create the 
cluster).  The shared variable will be created, readable/writable from
all threads.  The variable will be of class \code{big.matrix}; see 
the library \pkg{bigmemory} for details.
}

% \value{
% }

\author{
Norm Matloff
}

