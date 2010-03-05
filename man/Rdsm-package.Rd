\name{Rdsm}
\alias{Rdsm}

\title{Distributed Threads Programming in R}

\description{\pkg{Rdsm} provides a threads-like programming environment
   for \R, usable both on a multicore machine \emph{and} across a
   network of multiple machines.  The package gives the illusion of the
   shared memory world view that threads systems provide, again even 
   across multiple machines on a network.

   The ``dsm'' in ``Rdsm'' stands for \emph{distributed shared memory},
   a term from the parallel processing community in which nodes in a
   cluster share (real or conceptual) memory.  It is based on a similar
   package I wrote for Perl some years ago (Matloff (2002)).
   
   }

\section{Quick Introduction to \pkg{Rdsm}}{

   The \pkg{Rdsm} code in \code{MatMul.r} in the examples included in
   this package serves as as a quick introduction, using a
   matrix-multiply example common in parallel processing packages.
   There are especially detailed comments in this example. 

}

\section{Advantages of the Shared-Memory Paradigm}{

   Whether or not the platform is a multicore machine or a set of
   networked computers, a major advantage of \pkg{Rdsm} is that it gives the
   programmer a shared-memory world view, considered by many in the
   parallel processing community to be one of the clearest forms of
   parallel programming (Chandra (2001), Hess \emph{et al} (2003) etc.).

   Suppose for instance we wish to copy \code{x} to \code{y}.  In a
   message-passing setting such as \pkg{Rmpi}, \code{x} and \code{y} may
   reside in processes 2 and 5, say.  The programmer would write code
   (described here in pseudocode)

   \code{send x to process 5}
   
   to run on process 2, and write code
   
   \code{receive data item from process 2}

   \code{set y to received item}
   
   to run on process 5.  By contrast, in a shared-memory environment,
   the programmer would merely write
   
   \code{y <- x}
   
   which is vastly simpler.
   
   This also means that it is easy to convert sequential \R
   code to parallel \pkg{Rdsm} code.  

   Packages such as \pkg{snow}, arguably in the message-passing realm,
   do feature more convenient messaging operations, but still shared
   memory tends to have the simplest code.  
   
   (It should be noted, though, that in some applications
   message-passing can yield somewhat better performance.)

}

\section{Types of Applications}{

   \pkg{Rdsm} should have a wide variety of applications, such as
   
   \itemize{
   
   \item Performance programming, in ``embarrassingly parallel''
   settings (such as \file{Disc.R} in the examples included with this
   package).
   
   \item Parallel I/O applications, e.g. parallel Web data collection
   (such as \file{WebProbe.R} in the examples included with this
   package).
   
   \item Collaborative tools (such as \file{Auction.R} in the examples
   included with this package).
   
   }

Most of these applications can also be done via non-shared memory
paradigms, but the shared-memory paradigm makes the applications easier
to develop, maintain and extend.

}

\section{Communication}{

\pkg{Rdsm} runs via network sockets.  

Data communication is binary in the case of vectors and matrices for
speed, but \code{serialize} and \code{unserialize} are used for lists.

}

\section{Launching \pkg{Rdsm}}{

   To run \pkg{Rdsm} manually, run \R in n+1 different terminal (shell)
   windows, where n is the desired number of clients, i.e. degree of
   parallelism.  One of those windows will be used for the server.

   Then:
   
   \itemize{

      \item Run \code{srvr()} in your server window.  (Or run
      \code{srvrinit()} and then \code{srvrloop()}; \code{srvr()}
      is merely a convenience that calls both.)

      \item In each client window, run \code{init()}.
      
      \item In each client window, run your \pkg{Rdsm} application
      function.  
 
   }
 
   You may have several application functions to run, or may want to run
   the same one multiple times.  This is fine as long as you don't rerun
   \code{srvrinit()}.  You must still rerun \code{srvrloop}, but you
   do not need to rerun \code{init} on the client side.
   Application-program \pkg{Rdsm} variables etc. will be retained from
   one run to the next.

   The script \file{AutoLaunch.R} automates the above process.  One
   opens just one window, and the script automatically creates windows
   for the server and clients.  Then each time the user wishes to issue
   a command to all the clients, he/she merely types the command in the
   original window, and it will be sent to the client windows, thus
   saving a lot of typing.

   The main functions available in \file{AutoLaunch.R} are:
   
   \itemize{

      \item \code{alint(nclnt=NULL,nds=NULL)}

      Call this to create the client windows, and start an \R process in
      each one.  If \code{nds} is NULL, then this sets up \code{nclnt} 
      windows/\R processes on \code{localhost}, i.e. the machine on
      which the script is run.

      \item \code{setldir(libdir)}

      This adds \code{libdir} to the \R library search path at the
      server and clients.

      \item \code{setdir(cdir=NULL)}

      This changes the working directory to \code{cdir} at each client.
      If \code{cdir} is NULL, then the client directories are changed to
      the current working directory of the \R process runing this
      script.

      \item \code{alinits(nclnt=NULL,nds=NULL)}

      This combines \code{alinit} and \code{setdir} (with \code{cdir}
      NULL), as a convenience.

      \item \code{cmdtoclnts(cmd)}

      This sends the command \code{cmd}, single-quoted, to all the
      clients.

      \item \code{loadrdsm()}

      This loads \pkg{Rdsm} at the server and the clients.

      \item \code{cmdtosrvr(cmd)}

      This sends the command \code{cmd}, single-quoted, to the
      server.

      \item \code{cmdtoall(cmd)}

      This sends the command \code{cmd}, single-quoted, to the server
      and all the clients.

      \item \code{alquit()}

      This call causes the \R processes at the server and all the
      clients to exit, and the corresponding windows to close.

   }
   
}

\section{Accessing \pkg{Rdsm} Variables}{

   The variables in a typical \pkg{Rdsm} application program consist of
   a few \pkg{Rdsm} variables and many ``ordinary'' variables.  Ordinary
   \R syntax is used to access the former, just as the latter.

   For example, suppose your \pkg{Rdsm} program includes \code{m}, a 4x5
   matrix variable of class \code{dsmm}.  If you wished to fill the
   second column with 1, 2, 3 and 4, you would write
   
   \preformatted{ m[,2] <- 1:4 }
   
   just as you would in ordinary \R.  

   Note carefully that you must always use brackets with \pkg{Rdsm}
   variables.  For instance, to copy the \pkg{Rdsm} vector \code{x} to
   an ordinary \R variable \code{y}, write
   
   \preformatted{ y <- x[] }

   not
   
   \preformatted{ y <- x }

}

\section{Built-in Variables}{

   \pkg{Rdsm}'s built-in variables are stored in a single global variable
   \code{myinfo}, a list consisting of these components:
   
   \itemize{
   
      \item \code{myid}: the ID number of this client, starting with 1
      
      \item \code{nclnt}: the total number of clients
   
   }

}

\section{Built-in Synchronization Functions}{

   \pkg{Rdsm} includes some built-in synchronization functions similar to those
   of threaded or other shared-memory programming systems:
   
   \itemize{
      \item \code{barr()}: barrier operation
      \item \code{lock()}: lock operation
      \item \code{unlock()}: unlock operation
      \item \code{wait()}: wait operation
      \item \code{signal()}: signal operation
      \item \code{fa()}: fetch-and-add operation
      \item \code{rpc()}: remote procedure call operation
   }
   
}

\section{Built-in Initialization/Shutdown Functions}{

   \itemize{
      \item \code{init()}:  initializes a client's connection to the server
      \item \code{srvrinit()}:  initializes the server
      \item \code{srvrloop()}:  runs the server
      \item \code{srvr()}:  calls both \code{srvrinit()} and \code{srvrloop()}
      \item \code{dsmexit()}:  called when a client has finished its work
   }

}

\section{Internal Structure}{

   Though transparent to the \pkg{Rdsm} programmer, internally \pkg{Rdsm} has the
   following architecture.

   The \pkg{Rdsm} application variables reside on the server.  Each read from
   or write to an \pkg{Rdsm} variable involves a transaction with the server.

   \pkg{Rdsm} variables reference vectors, matrices and lists, but have
   the special \pkg{Rdsm} classes \code{dsmv}, \code{dsmm} and
   \code{dsml}, respectively.  Indexing operations for these classes
   communicate with the server to read or write the desired objects.
   
   Again, all this is transparent to the \pkg{Rdsm} programmer.
   However, as with any system, a good understanding of the internals
   can result in your writing much better code.

}

\section{High-Performance Computing}{

   \pkg{Rdsm} can do quite well in \emph{embarrassingly parallel}
   applications, meaning those in which the task at hand can easily be
   broken in smaller independent tasks to be done by the processes.  An
   additional requirement is that the ``output,'' i.e. \pkg{Rdsm} variables
   used to contain results of the large task, not be voluminous or
   frequently written.

   It should be noted that this is not just an issue with \pkg{Rdsm}.  Indeed,
   the functional programming nature of \R, in which writing to just one
   element of a vector or array means rewriting the entire data object,
   essentially rules out good performance in non-embarrassingly parallel
   problems.  On multicore/multiprocessor systems, I recommend
   interfacing \R with C/C++ in OpenMP, with OpenMP doing the parallel
   portion of your work.

}

\section{Relation to \pkg{bigmemory}}{

The other \R package providing a shared-memory paradigm (read/write) is
\pkg{bigmemory}.  That package is restricted to multicore platforms,
while \pkg{Rdsm} is much more general, usable on sets of separate but
networked machines.  \pkg{Rdsm} also offers vector and list types in
addition to matrices, while \pkg{bigmemory} has only matrices.  On the
other hand, on multicore platforms, \pkg{bigmemory} may be faster for
some applications.

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
