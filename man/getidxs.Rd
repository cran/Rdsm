\name{getidxs}
\alias{getidxs}

\title{
Parallelizing work assignment.
}

\description{
Breaks a range of values into r vectors, where r is the number of
threads, for the purpose of partitioning work to the threads.
}

\usage{
getidxs(m)
}

\arguments{
  \item{m}{
  The sequence 1:m will be partitioned.
  }
}

\details{
The range 1:m will be partitioned into r subranges, the i-th of which
will be used by thread i to determine which work that thread has been
assigned.
}

\value{
The subrange assigned to the invoking thread.
}

\author{
Norm Matloff
}

