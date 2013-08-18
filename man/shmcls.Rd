\name{shmcls}
\alias{shmcls}

\title{
Create \pkg{snow} cluster
}

\description{
Convenience wrapper for setting up a \pkg{snow} cluster on a multicore
machine.
}

\usage{
shmcls(ncores)
}

\arguments{
  \item{ncores}{
  The number of cores to use in the cluster.
  }
}

\details{
A \pkg{snow} cluster of \code{ncores} will be created.
}

\value{
The cluster object.
}

\author{
Norm Matloff
}

