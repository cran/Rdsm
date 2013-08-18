
# Rdsm worker/manager code

# author:  N. Matloff

# builds on R's Snow ("parallel") and bigmemory packages to form a
# threads type of environment on shared memory machines (or, using file
# storage for sharing; that case is usually not mentioned below, for
# brevity); each Snow worker runs one thread

# shared variables are set up in physical shared memory; due to
# bigmemory restriction, all shared variables are matrices; accessed via
# the name, no quotes, e.g.
#    mgrmakevar(cls,"w",1000,1000)  # run at manager
#    w[2,5] <- 8  # run at worker, then 8 visible at other threads and mgr.

# initializing Rdsm:
# 
# start the R "parallel" library (uses Snow part of "parallel")
# create a Snow cluster, cls 
# run mgrinit(cls) to initialize Rdsm

# running an Rdsm app:
#
# make shared variables for the app, using mgrmakevar()
# run desired user Rdsm code for the app, using clusterEvalQ() to launch
# threads running that code

if(getRversion() >= "2.15.1") globalVariables(
   c("brlock","realrdsmlock","myinfo","realrdsmunlock","barrlock"))

# options(bigmemory.typecast.warning=FALSE)

# *************************  INITIALIZATION  ***********************

# manager init, starts Rdsm on the Snow cluster cls; if boost, then uses
# synchronicity locks, else use backing store locks; the argument back
# specifies whether certain variables involved with the barrier are to
# be stored in backing store rather than in memory

mgrinit <- function(cls,boost=F,back=F) {
   # set up so that each worker node will have a global variable myinfo
   # that contains the thread ID and number of threads
   setmyinfo <- function(i,n) {
      assign("myinfo",list(id = i, nwrkrs = n),pos=tmpenv)
   }
   ncls <- length(cls)
   parallel::clusterEvalQ(cls,tmpenv <- new.env())
   parallel::clusterApply(cls,1:ncls,setmyinfo,ncls)
   parallel::clusterEvalQ(cls,myinfo <- get("myinfo",tmpenv))
   # set up the requested locking type 
   if (boost) {
      rdsmlock <- boostlock
      rdsmunlock <- boostunlock
   } else {
      rdsmlock <- backlock
      rdsmunlock <- backunlock
   }
   # set up the workers to use the proper lock type
   parallel::clusterExport(cls,"rdsmlock",envir=environment())
   parallel::clusterExport(cls,"rdsmunlock",envir=environment())
   parallel::clusterEvalQ(cls,realrdsmlock <- rdsmlock)
   parallel::clusterEvalQ(cls,realrdsmunlock <- rdsmunlock)
   # send the threads needed Rdsm functions
   parallel::clusterExport(cls,"barr")
   parallel::clusterExport(cls,"getidxs")
   # make a single barrier, set it up on the worker nodes
   makebarr(cls,boost,back)
}

# *************************  CREATE SHARED VARIABLES  ***********************

# mgrmakevar() is executed from manager

# storage type is memory if back = F; back = T means file storage

# the variable is created in the global space of the worker nodes; at
# the manager, the variable is created in the environment of the caller,
# or not at all, depending on mgrcpy

mgrmakevar <- function(cls,varname,nr,nc,vartype="double",
      back=F,mgrcpy=T) {
   tmp <- if (!back) 
      bigmemory::big.matrix(nrow=nr,ncol=nc,type=vartype) else
      bigmemory::filebacked.big.matrix(nrow=nr,ncol=nc,
         type=vartype,backingfile=varname) 
   # make accessible to manager
   if (mgrcpy) assign(varname,tmp,pos=parent.frame())  
   # get the descriptor for this big.matrix object, to send to the
   # worker nodes
   parallel::clusterExport(cls,"varname",envir=environment())
   if (!back) {
      desc <- bigmemory::describe(tmp)
      parallel::clusterExport(cls,"desc",envir=environment())
      parallel::clusterEvalQ(cls,
         tmp <- bigmemory::attach.big.matrix(desc))
   } else {
      parallel::clusterEvalQ(cls,
         tmp <- bigmemory::attach.big.matrix(paste(varname,".desc",sep="")))
   }
   parallel::clusterEvalQ(cls,assign(varname,tmp))  
   invisible(0)
}

# *************************  LOCKS  ***********************

# these are just stubs, to be replaced at runtime by the requested locks
# type
rdsmlock <- function(lck) 0
rdsmunlock <- function(lck) 0

mgrmakelock <- function(cls,lockname,boost=F) {
   if (boost) {
      require(synchronicity)
      tmp <- synchronicity::boost.mutex()
      desc <- synchronicity::describe(tmp)
      parallel::clusterEvalQ(cls,require(synchronicity))
      parallel::clusterExport(cls,"desc",envir=environment())
      parallel::clusterEvalQ(cls,
         tmp <- synchronicity::attach.mutex(desc)) 
      parallel::clusterExport(cls,"lockname",envir=environment())
      parallel::clusterEvalQ(cls,assign(lockname,tmp))  
   } else {
      # make sure lock dir not left over from a previous run
      unlink(lockname,recursive=T)
   }
}

# synchronicity lock/unlock
boostlock <- function(lck) {
   require(synchronicity)
   synchronicity::lock(lck)  
}
boostunlock <- function(lck) {
   require(synchronicity)
   synchronicity::unlock(lck)  
}

# general lock/unlock (writing to a shared filesystem)
# lockname must be quoted
backlock <- function(lockname) {
   repeat {
      if (dir.create(lockname)) return()
   }
}
backunlock <- function(lockname) {
   unlink(lockname,recursive=T)
}

# *************************  BARRIER  ***********************

# sense-reversing barrier implementation; see mgrinit() above regarding
# boost
makebarr <- function(cls,boost=F,back=F) {
   mgrmakevar(cls,"barrnumleft",1,1,"integer",back=back,mgrcpy=F)
   mgrmakevar(cls,"barrsense",1,1,"integer",back=back,mgrcpy=F)
   mgrmakelock(cls,"barrlock",boost)
   if (boost) {
      clusterEvalQ(cls,brlock <- barrlock)
   } else {
      clusterEvalQ(cls,brlock <- "barrlock")
   }
   # first component is the count, second is "parity";
   # barrnumleft is number left before barrier done
   clusterEvalQ(cls,barrnumleft[1] <- myinfo$nwrkrs)  
   clusterEvalQ(cls,barrsense[1] <- 0)  # sense (0 or 1)
}

# barrier op
barr <- function() {
   realrdsmlock(brlock)
   count <- barrnumleft[1]
   sense <- barrsense[1]
   if (count == 1) {  # all done
      # reset count
      barrnumleft[1] <- myinfo$nwrkrs 
      # reverse sense
      barrsense[1] <- 1 - barrsense[1]
      realrdsmunlock(brlock)
      return()
   } else {
      barrnumleft[1] <- barrnumleft[1] - 1
      realrdsmunlock(brlock)
      repeat {
         if (barrsense[1] != sense) break
      }
   }
}

# *************************  UTILITIES  ***********************

# convenience function to set up Snow cluster on multicore machine
shmcls <- function (ncores)
{  return(parallel::makeCluster(type = "PSOCK", rep("localhost", ncores)))
}

# find indices among 1:m to be handled by the given node
getidxs <- function(m) {
   parallel::splitIndices(m,myinfo$nwrkrs)[[myinfo$id]]
}

# to shut down cluster (important!):
# stopCluster(cls)

