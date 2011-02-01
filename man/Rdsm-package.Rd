\name{Rdsm}
\alias{Rdsm} 

\title{Threads Programming for R} 

\description{\pkg{Rdsm} provides a threads programming environment
   for \R, not available within \R itself.  Moreover, it is
   usable both on a multicore machine \emph{and} across a
   network of multiple machines.  The package gives the ``look and
   feel'' of the shared memory world view that ordinary system 
   threads provide, again even across multiple machines on a network.

   The ``dsm'' in ``Rdsm'' stands for \emph{distributed shared memory},
   a term from the parallel processing community in which nodes in a
   cluster share (real or conceptual) memory.  It is based on a similar
   package the author wrote for Perl some years ago (Matloff (2002)).

    \pkg{Rdsm} can be used for:
   
      \itemize{

         \item parallel computation, as with the program \file{KNN.R}
         included with this package

         \item the development of ``dashboard'' controllers and parallel
         I/O, like the program \file{WebProbe.R}

         \item the development of collaborative tools, as with the
         program \file{Auction.R}

      }
    
   \pkg{Rdsm} can easily be used with variables produced by Jay Emerson
   and Mike Kane's \pkg{bigmemory} package, thus enhancing the latter
   package by adding a threads capability.  In \pkg{bigmemory} case, if
   the code run on a multicore machine, then the shared memory is real,
   and the access may be considerably faster than to \pkg{Rdsm}
   variables.  \pkg{Rdsm} provides a function \code{newbm()} for
   creating \pkg{bigmemory} variables.
   
   }

\section{Quick Introduction to \pkg{Rdsm}}{

   The \pkg{Rdsm} code in \file{MatMul.R} in the examples included in
   this package serves as a quick introduction, using a
   matrix-multiply example common in parallel processing packages.
   There are especially detailed comments in this example, but here is
   an overview:

   The code finds the product of matrices \code{m1} and \code{m2}, placing
   the produce in \code{prd}.  The core lines of the code are

   \preformatted{
   myid <- myinfo$myid  # this thread's ID
   # determine number of columns of m1
   k <- if(class(m1) == "big.matrix") dim(m1)[2] else m1$size[2] 
   nth <- myinfo$nclnt  # number of threads
   chunksize <- k/nth
   # determine which columns of m1 this thread will process
   firstcol <- 1 + (myid-1) * chunksize
   lastcol <- firstcol + chunksize - 1
   # process this thread's share of the columns
   prd[,firstcol:lastcol] <- m1[,] \%*\% m2[,firstcol:lastcol]
   }

   The work is parallelized by assigning each thread a certain set of
   columns of \code{prd}.  Each thread then computes its columns and
   then places them in the proper section of \code{prd}.  This is a
   classical shared-memory pattern, thus illustrating the point that
   \pkg{Rdsm} brings threads programming to \R.  
   
   The matrix \code{prd} here is a shared variable, created beforehand
   via a call to \code{cnewdsm()} in the case of an \pkg{Rdsm} variable
   or via a call to \code{newbm()} if a \pkg{bigmemory} variable is
   desired.

   Other examples, including directions for running them, are given in
   the \file{examples/} and \file{testscripts/} directories in this
   package.

}

\section{Advantages of the Shared-Memory Paradigm}{

   Whether the platform is a multicore machine or a set of
   networked computers, a major advantage of \pkg{Rdsm} is that it gives the
   programmer a shared-memory world view, considered by many in the
   parallel processing community to be one of the clearest forms of
   parallel programming (Chandra (2001), Hess \emph{et al} (2003) etc.).

   Suppose for instance we wish to copy \code{x} to \code{y}.  In a
   message-passing setting such as \pkg{Rmpi}, \code{x} and \code{y} may
   reside in processes 2 and 5, say.  The programmer would write code
   (described here in pseudocode)

   \preformatted{
      send x to process 5
   }

   to run on process 2, and write code
   
   \preformatted{
      receive data item from process 2
      set y to received item
   }
   
   to run on process 5.  By contrast, in a shared-memory environment,
   the programmer would merely write
   
   \preformatted{
      y <- x
   }
   
   which is vastly simpler.  (Brackets would be required, as
   explained below.)
   
   This also means that it is easy to convert sequential \R
   code to parallel \pkg{Rdsm} code.  

   Packages such as \pkg{snow}, arguably in the message-passing realm,
   do feature more convenient messaging operations, but still shared
   memory tends to have the simplest code.  
   
   (It should be noted, though, that in some applications
   message-passing can yield somewhat better performance.)

}

\section{Communication}{

\pkg{Rdsm} runs via network sockets, and \pkg{Rdsm} shared variables are
accessed via this mechanism.  If one's code also contains \pkg{bigmemory}
shared variables, these are handled in that package's environment,
either physical shared memory or file backing in a shared file system.

\pkg{Rdsm} data communication is binary in the case of vectors and
matrices, but \code{serialize()} and \code{unserialize()} are used for
lists.

}

\section{Launching \pkg{Rdsm}}{

Start R and load \pkg{Rdsm}.

\emph{Manual operation:}

   To run \pkg{Rdsm} manually, run \R in n+1 different terminal (shell)
   windows, where n is the desired number of clients, i.e. degree of
   parallelism.  Each client runs one thread.  You will use one of the
   n+1 instances of \R for the server.

   Then:
   
   \itemize{

      \item Run \code{srvr()} in your server window, with argument n,
      which is 2 by default.  

      \item In each client window, run \code{init()}.
      
      \item In each client window, run your \pkg{Rdsm} application
      function.  
 
   }
 
   You may have several application functions to run, or may want to run
   the same one multiple times.  This is fine as long as \code{srvr()}
   is still running; you do not need to rerun \code{init()} at the
   clients.  Application-program \pkg{Rdsm} variables etc. will be
   retained from one run to the next.

   \emph{Automatic launching:}

   If you are running on a Unix-family system (Linux, Mac OS, or Cygwin
   on Windows), \pkg{Rdsm} launch and management can be made much more
   convenient via \pkg{Rdsm}'s autolaunch capability.  One opens just
   one window, and autolaunch automatically creates windows for the
   server and clients, and then in each window starts \R and loads
   \pkg{Rdsm} (and optionally \pkg{bigmemory}).

   Then each time the user wishes to issue a command to all the clients,
   say a command to run an \pkg{Rdsm} application, he/she merely types
   the command in the original window, and it will be sent to the client
   windows, thus saving a lot of typing.

   Here's a quick summary example of autolaunch.  Say we wish to run two
   threads, with our application consisting of a function \code{x()}
   contained in the source code file \file{y.R}.  We would open a single
   terminal window, run \R in it, and then run the following code:

   \preformatted{
       alinit(2)  # create clients
       cmdtoclnts('source("y.R")')  # have clients source the app code
       go()  # set up server/client connections
       cmdtoclnts('x(3,100)')  # first run of app
       cmdtoclnts('x(12,5000)')  # second run of app
       ...
   }

   Here's what it does:

   \itemize{

      \item The call to \code{alinit()} opens two other terminal
      windows, starts \R in them, and loads the \pkg{Rdsm}
      library.  

      \item The call to \code{cmdtoclnts()} then has the instances of \R
      at the client windows load our application source file.

      \item The call to \code{go()} then starts \code{srvr()} in the
      server window and \code{init()} in each client window.  

      \item We then run our application a couple of times, and of course
      could run other \pkg{Rdsm} applications after sourcing their code.

   }

}

\section{Accessing \pkg{Rdsm} Variables}{

   The variables in a typical \pkg{Rdsm} application program consist of
   a few shared variables, produced by either \pkg{Rdsm} or
   \pkg{bigmemory},  and many ``ordinary'' variables.  Regular \R
   syntax is used to access the shared variables, just as with the
   ordinary ones.

   For example, suppose your program includes \code{m}, a 4x5 shared
   matrix variable.  If you wished to fill the second column with 1, 2,
   3 and 4, you would write
   
   \preformatted{ m[,2] <- 1:4 }
   
   just as you would in ordinary \R.  

   Note carefully that you must always use brackets with shared
   variables.  For instance, to copy the shared vector \code{x} to
   an ordinary \R variable \code{y}, write
   
   \preformatted{ y <- x[] }

   not
   
   \preformatted{ y <- x }

}

\section{Built-in Variables}{

   \pkg{Rdsm}'s built-in variables are stored in a single global (but
   not shared) variable \code{myinfo}, a list consisting of these components:
   
   \itemize{
   
      \item \code{myid}: the ID number of this client, starting with 1
      
      \item \code{nclnt}: the total number of clients
   
   }

}

\section{Built-in Synchronization Functions}{

   \pkg{Rdsm} includes some built-in synchronization functions similar to those
   of threaded or other shared-memory programming systems:
   
   \itemize{
      \item \code{barr()}: barrier operation, synchs all threads to the
         same code line
      \item \code{lock()}: lock operation, gives thread exclusive access
         to shared variables
      \item \code{unlock()}: unlock operation, relinquishes exclusive
         access
      \item \code{wait()}: wait operation
      \item \code{signal()}: signal operation; releases all waiting
         clients
      \item \code{signal1()}: same as \code{signal()}, but releases only
         the first waiting client
      \item \code{fa()}: fetch-and-add operation
   }
   
}

\section{Built-in Initialization/Shutdown Functions}{

   \itemize{
      \item \code{init()}:  initializes a client's connection to the server
      \item \code{srvr()}:  initializes the server
      \item \code{dsmexit()}:  can be called when a client has finished its
         work (note: this will stop the server when all clients make this
         call, and thus this function should not be used in most
         applications)
   }

}

\section{Shared-Variable Creation Functions}{

   \itemize{
      \item \code{cnewdsm()}:  creates an \pkg{Rdsm} variable
      \item \code{newbm()}:  creates a \pkg{bigmemory} variable
   }
}

\section{Internal Structure}{

   Though transparent to the \pkg{Rdsm} programmer, internally
   \pkg{Rdsm} variables (but not \pkg{bigmemory} ones) have
   the following architecture.

   The \pkg{Rdsm} application variables reside on the server.  Each read from
   or write to an \pkg{Rdsm} variable involves a transaction with the server.
   \pkg{Rdsm} variables reference vectors, matrices and lists, but have
   the special \pkg{Rdsm} classes \code{dsmv}, \code{dsmm} and
   \code{dsml}, respectively.  Indexing operations for these classes
   communicate with the server to read or write the desired objects.

   See the \pkg{bigmemory} package for details of the structure used for
   those variables.  These are of the matrix type only, class
   \code{big.matrix}.  Of course, a vector can be represented as a
   one-row vector.
   
   Again, all this is transparent to the programmer.  However, as with
   any system, a good understanding of the internals can result in your
   writing much better code.

}

\author{Norm Matloff}

\references{

Chandra, Rohit (2001), \emph{Parallel Programming in OpenMP}, Kaufmann,
pp.10ff (especially Table 1.1).

Hess, Matthias \emph{et al} (2003), Experiences Using OpenMP Based on Compiler
Directive Software DSM on a PC Cluster, in \emph{OpenMP Shared Memory
Parallel Programming: International Workshop on OpenMP Applications and
Tools}, Michael Voss (ed.), Springer, p.216.

Matloff, Norman (2002), PerlDSM: A Distributed Shared Memory System for
Perl.  \emph{Proceedings of PDPTA 2002}, 2002, 63-68. 

}
