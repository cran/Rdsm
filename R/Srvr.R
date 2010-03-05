# server code

# author:  N. Matloff

# server init; handles ncon clients 
srvrinit <- function(port=2000,ncon=2) {
   ncon <<- ncon
   glbls <<- list()  # Rdsm application variables
   barrvar <<- 0  # initialize barrier
   locks <<- list()  # no locks at first
   waitvars <<- list()  # no wait variables at first
   # need to set up socket connections with clients
   cons <<- vector(mode="list",length=ncon)  # list of connections
   # prevent connection from dying during debug or long compute spell
   options("timeout"=10000)  
   # receive and establish client connection requests
   for (i in 1:ncon) {
      cons[[i]] <<-
         socketConnection(port=port,server=TRUE,blocking=TRUE,open="a+b")
      # wait to hear from client i
      checkin <- unserialize(cons[[i]])
   }
   # send ACKs 
   for (i in 1:ncon) {
      # send the client its ID number, and the group size
      serialize(c(i,ncon),cons[[i]])
   }
   remainingclients <<- ncon
}

# assuming srvrinit() has already been executed, this runs the main loop
# of the server; it runs until no clients are left
srvrloop <- function() {
   # main loop; each iteration of the loop will service one client
   # request
   # in case of execution error, re-inits some srvrinit() variables,
   # so don't have to run latter again
   on.exit({barrvar <<- 0; locks <<- list(); waitvars <- list()})  
   repeat {
      # any clients still there?
      if (remainingclients == 0) break
      # find all the pending client requests
      rdy <- which(socketSelect(cons))
      # choose one
      j <- sample(1:length(rdy),1)
      con <- cons[[rdy[j]]]  
      # read client request
      req <- unserialize(con)
      reqtype <- req$reqtype
      req$reqtype <- NULL
      switch(reqtype,
         "new"=donewdsm(req,con),
         "get"=dsmget(req,con), 
         "put"=dsmput(req,con), 
         "barrinc"=barrinc(),
         "lock"=dolock(req$lockname,con),
         "unlock"=dounlock(req$lockname),
         "wait"=dowait(req$waitvarname,con),
         "signal"=dosignal(req$waitvarname),
         "fa"=dofa(req,con),
         "newf"=donewf(req),
         "rpc"=dorpc(req,con),
         "quit"=quit(con))
   }
   # reset in case wish to do another Rdsm run while retaining the
   # current user variables, etc.
   remainingclients <<- ncon
}

# for convenience
srvr <- function(port=2000,ncon=2) {
   srvrinit(port=port,ncon=ncon)
   srvrloop()
}

# read sz elements of mode md (integer of double) from cn
srvrbinread <- function(cn,md,sz) {
   return(readBin(con=cn,what=md,n=sz))
}

# write dt, of mode md (integer of double), to cn
srvrbinwrite <- function(dt,md,cn) {
   writeBin(dt,con=cn)
}

# read an R object from cn
srvrreadobj <- function(cn) {
   return(unserialize(cn))
}

# write an R object to cn
srvrwriteobj <- function(robj,cn) {
   serialize(robj,cn)
}

# create a new Rdsm variable
donewdsm <- function(rq,cn) {
   rq$req <- NULL
   varname <- rq$varname
   glbls[[varname]] <<- rq
   if (class(rq) == "dsml") {
      val <- srvrreadobj(cn)
   } else val <- srvrbinread(cn,rq$md,rq$size)
   # value arrives as a list/vector, so must change to matrix if needed
   if(class(rq) == "dsmm") 
      val <- as.matrix(val,nrow=glbls[[varname]]$size[[1]])
   glbls[[varname]]$val <<- val
}

# client wants to read an Rdsm variable
dsmget <- function(rq,cn) {
   subs <- rq$subs
   varname <- rq$varname
   thisglbl <- glbls[[varname]]
   if (class(rq) == "dsml") {
      srvrwriteobj(thisglbl$val[subs],cn)
   } else srvrbinwrite(thisglbl$val[subs],thisglbl$md,cn)
}

# client wants to write an Rdsm variable
dsmput <- function(rq,cn) {
   subs <- rq$subs
   varname <- rq$varname
   if (class(rq) == "dsml") {
      glbls[[varname]]$val[subs] <<- srvrreadobj(cn)
   } else {
      n <- length(subs)
      glbls[[varname]]$val[subs] <<- srvrbinread(cn,glbls[[varname]]$md,n)
   }
}

# barrier handler
# with just a single server, no need to use an "even/odd" alternating
# barrier
barrinc <- function() {
   barrvar <<- barrvar + 1
   if (barrvar == ncon) {
      for (con in cons) srvrwriteobj("resume",con)
      barrvar <<- 0
   }
}

# lock handler; see comments at top of this file
dolock <- function(lockname,cn) {
   if (length(locks[[lockname]]) == 0)  {  # lock is unlocked
      locks[[lockname]] <<- list(cn)
      srvrwriteobj("resume",cn)
   } else {  # lock is locked, so join the queue
      lng <- length(locks[[lockname]])
      locks[[lockname]][[lng+1]] <<- cn
   }
}

# unlock handler; see comments at top of this file
dounlock <- function(lockname) {
   # current connection done with lock, so remove 
   locks[[lockname]] <<- locks[[lockname]][-1]  
   if (length(locks[[lockname]]) > 0)  # others are waiting for lock 
      srvrwriteobj("resume",locks[[lockname]][[1]])
}

# wait handler; see comments at top of this file
dowait <- function(waitvarname,cn) {
   # join group of waiting connections
   if (length(waitvars[[waitvarname]]) == 0)  {  
      waitvars[[waitvarname]] <<- list(cn)
   } else {
      waitvars[[waitvarname]] <<- c(waitvars[[waitvarname]],list(cn))
   }
}

# signal handler; see comments at top of this file
dosignal <- function(waitvarname) {
   # signal all who are waiting
   for (cn in waitvars[[waitvarname]]) {
      srvrwriteobj("resume",cn)
   }
   waitvars[[waitvarname]] <<- NULL
}

# fetch-and-add; see fa() in Clnt.R
dofa <- function(rq,cn) {
   oldval <- glbls[[rq$fav]]$val
   glbls[[rq$fav]]$val <<- oldval + rq$inc
   srvrwriteobj(oldval,cn)
}

# receive new function from client, for use in Remote Procedure Calls
donewf <- function(rq) {
   ftnname <- rq$ftnname
   glbls[[ftnname]] <<- rq$ftn
}

# Remote Procedure Call; see Clnt.R comments for restrictions
dorpc <- function(rq,cn) {
   ftn <- glbls[[rq$ftnname]]
   rslt <- do.call(ftn,rq$arglist)
   srvrwriteobj(rslt,cn)
}

# client cn quits
quit <- function(cn) {
   remainingclients <<- remainingclients - 1
}
