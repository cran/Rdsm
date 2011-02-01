# Rdsm client/server code, plus autolaunch routines

# author:  N. Matloff

###########################################################################
###########################################################################
################        client routines       #############################
###########################################################################
###########################################################################

# client initialization; connect to server, set myinfo 
init <- function(host="localhost",port=2000) {
   # prevent connection from dying during debug or long compute spell
   options("timeout"=10000)  
   # connect to server 
   con <- socketConnection(host=host,port=port,blocking=TRUE,open="a+b")
   serialize(list(req="checking in"),con)
   # receive this client's ID and total number of clients from server
   myidandnclnt <- unserialize(con)  
   myinfo <<- list(con=con,myid=myidandnclnt[1],nclnt=myidandnclnt[2])
}

# constructor for an Rdsm variable, creating object with quoted name
# varname, mode thismode, value val, class thisclass ("dsmv","dsmm" or
# "dsml")
# one client, say client 1, sends to server, with non-NULL val, NULL
# size, and then registers this variable at the server; the other
# clients have NULL val, non-NULL size (except dsml), and register the
# variable only locally
newdsm <- function(varname,thisclass,thismode=NULL,val=NULL,size=NULL) {
   # build the local object
   newvar <- list()
   newvar$varname <- varname
   newvar$md <- thismode
   # determine size of value to be stored, and all its subscripts
   if (!is.null(val)) {
      if (thisclass=="dsmv" || thisclass=="dsml") {  # vector or list
         newvar$size <- length(val) 
         subs <- 1:length(val)
      } else {  # matrix
         newvar$size <- dim(val)
         subs <- list(1:nrow(val),1:ncol(val))
      }
   } else newvar$size <- size
   class(newvar) <- thisclass
   assign(varname,newvar,pos=.GlobalEnv)  # register locally
   # if client 1 (see above), send to server 
   if (!is.null(val)) 
      sendvml(newvar,val,"new",subs)
}

# convenient version of newdsm(), for cases in which val is a constant,
# rather than a variable (or is a variable whose value is the same at
# all nodes); thismode should be left as NULL in the case dsml
cnewdsm <- function(varname,thisclass,thismode=NULL,val) {
   if (myinfo$myid == 1) {
      newdsm(varname,thisclass,thismode,val=val)
   } else {
      if (thisclass == "dsml") size <- length(val) else
      size <- 
         if (thisclass == "dsmv") length(val) else dim(val)
      newdsm(varname,thisclass,thismode,size=size)
   }
}

# create a new bigmemory variable, named varname of mode thismode; the
# matrix is created at node 1, then attached at the other nodes;
# optional initial value is val
# bigmemory library must be present
newbm <- function(varname,thismode,nr,nc,val=NULL) {
   if (myinfo$myid == 1) {
      assign(varname,big.matrix(nrow=nr,ncol=nc,type=thismode),pos=.GlobalEnv)
      newdsm("bmdes","dsml",val=list(describe(get(varname))))
      if (!is.null(val)) {
         v <- get(varname)
         v[,] <- val
      }
      barr()
   } else {
      newdsm("bmdes","dsml",size=1)
      barr()
      assign(varname,attach.big.matrix(bmdes[][[1]]),pos=.GlobalEnv)
   }
}

# converts the two-dimensional subscripts of a submatrix to
# one-dimensional subscripts; s1 and s2 are the subscript ranges of rows
# and columns, and nr is the number of rows in the full matrix
twodim2onedim <- function(s1,s2,nr) {
   s1s2 <- expand.grid(s1,s2)
   return(apply(s1s2,1,ijn,nr))
}

# 2-d to 1-d subscripts; see twodim2onedim() above
ijn <- function(v,n) {
   return(n*(v[2]-1)+v[1])
}

# read sz items of mode md (integer or double) from server
binread <- function(md,sz) {
   return(readBin(con=myinfo$con,what=md,n=sz))
}

# write dt, of mode md (integer or double), to server
binwrite <- function(dt,md) {
   writeBin(dt,con=myinfo$con)
}

# read an R object from server
readobj <- function() {
   return(unserialize(myinfo$con))
}

# write an R object to server
writeobj <- function(robj) {
   serialize(robj,myinfo$con)
}

# send dsm object vml, in list or vector form (but may be a matrix)
sendvml <- function(vml,val,reqtype,subs) {
   # first warn server that this data is coming; in particular, it
   # must know the size, to know how much buffer space to allocate in
   # vector/matrix case
   tosrvr <- 
      list(varname=vml$varname,md=vml$md,size=length(val),reqtype=reqtype,
         subs=subs)
   class(tosrvr) <- class(vml)
   writeobj(tosrvr)
   # if dsml, just go ahead and send it
   if (class(vml) == "dsml") {
      writeobj(val)
      return()
   }
   # vector/matrix case
   # make sure correct mode; note: matrix becomes vector
   if(typeof(val) != vml$md) {
      asfn <- paste("as.",vml$md,sep="")
      val <- do.call(asfn,list(val))
   }
   if (is.matrix(val)) val <- as.vector(val)
   # now send the actual value to the server
   binwrite(val,vml$md)  # for now, don't account for endian issues
}

# receive dsm object vml, in list or vector form (but may be a matrix)
recvvml <- function(vml,subs) {
   # supply prelim info
   info <- list(varname=vml$varname,reqtype="get",subs=subs)
   class(info) <- class(vml)
   writeobj(info)
   # if dsml, just go ahead and receive it
   if (class(vml) == "dsml") return(readobj())
   # vector/matrix case
   size = if(!is.null(subs)) length(subs) else vml$size
   return(binread(vml$md,size))
}

# changes NULLs or negative values in subscript expression sb to form
# appropriate ranges
#
# note that sb could be subscripts for either a list/vector or one 
# dimension of a matrix
#
# upperlim is e.g. 3 for a vector whose full length is 3, 4 for a
# matrix will 4 rows for a row subscript and 5 for a matrix of 5 columns
# for a column subscript
replacenullneg <- function(sb,upperlim) {
   if (is.null(sb)) return(1:upperlim)
   if (sb[1] < 0) {  # if 1 elt neg then all are
      return((1:upperlim)[sb])
   }
   return(sb) 
}

# read operation on dsmv object v; receives values from server
"[.dsmv" <- function(v,subs=NULL) {
   subs <- replacenullneg(subs,v$size)
   return(recvvml(v,subs))
}

# write operation on dsmv object v; sends server values to be written 
# R requires that last arg for [<- generics be named "value"
"[<-.dsmv" <- function(v,subs=NULL,value) {
   subs <- replacenullneg(subs,v$size)
   sendvml(v,value,"put",subs)
   # need to restore this variable in client listing of app globals, 
   # since the return value of this function (and other put ops) will 
   # be reassigned by R to v's entry 
   return(v)
}

# read operation on dsm vector object m
"[.dsmm" <- function(m,subs1=NULL,subs2=NULL) {
   subs1 <- replacenullneg(subs1,m$size[1])
   subs2 <- replacenullneg(subs2,m$size[2])
   subs <- twodim2onedim(subs1,subs2,m$size[1])
   return(matrix(recvvml(m,subs),nrow=length(subs1)))
}

# write operation on dsmm object m; see note on value in dsmv put above
"[<-.dsmm" <- function(m,subs1=NULL,subs2=NULL,value) {
   value <- as.vector(value)
   subs1 <- replacenullneg(subs1,m$size[1])
   subs2 <- replacenullneg(subs2,m$size[2])
   subs <- twodim2onedim(subs1,subs2,m$size[1])
   sendvml(m,value,"put",subs)
   # see "restore" comments in "[<-.dsmv" above
   return(m)
}

# sublist read operation on dsml object l; receives sublist from server
"[.dsml" <- function(l,subs=NULL) {
   subs <- replacenullneg(subs,l$size)
   return(recvvml(l,subs))
}

# sublist write operation on dsml object l; sends server list to be
# written; R requires that last arg for [<- generics be named "value"
"[<-.dsml" <- function(l,subs=NULL,value) {
   subs <- replacenullneg(subs,l$size)
   sendvml(l,value,"put",subs)
   # need to restore this variable in client listing of app globals, 
   # since the return value of this function (and other put ops) will 
   # be reassigned by R to l's entry 
   return(l)
}

# element read operation on dsml object l; receives values from server
"[.dsml" <- function(l,subs=NULL) {
   subs <- replacenullneg(subs,l$size)
   return(recvvml(l,subs))
}
  
# fetch-and-add op on fav, increment value inc; atomically adds inc to
# the shared variable fav, and returns fav's old value; user must call
# newdsm() separately to create fav, an Rdsm vector of length 1
fa <- function(fav,inc) {
   tosrvr <- list(reqtype="fa",fav=fav,inc=inc)
   writeobj(tosrvr)
   # wait for old value
   oldfavval <- readobj()
   return(oldfavval)
}

# barrier operation
barr <- function() {
   tosrvr <- list(reqtype="barrinc")
   writeobj(tosrvr)
   # wait for resume signal
   readobj()
   return()
}

lock <- function(lockname) {
   tosrvr <- list(reqtype="lock",lockname=lockname)
   writeobj(tosrvr)
   # wait for resume signal
   readobj()
   return()
}

unlock <- function(lockname) {
   tosrvr <- list(reqtype="unlock",lockname=lockname)
   writeobj(tosrvr)
}

wait <- function(waitvarname) {
   tosrvr <- list(reqtype="wait",waitvarname=waitvarname)
   writeobj(tosrvr)
   # wait for resume signal
   readobj()
   return()
}

signal <- function(waitvarname) {
   tosrvr <- list(reqtype="signal",waitvarname=waitvarname)
   writeobj(tosrvr)
}

signal1 <- function(waitvarname) {
   tosrvr <- list(reqtype="signal1",waitvarname=waitvarname)
   writeobj(tosrvr)
}

# close this client's connection
closecon <- function() close(myinfo$con)

# quit
dsmexit <- function() {
   tosrvr <- list(reqtype="quit")
   writeobj(tosrvr)
}

###########################################################################
###########################################################################
################        server routines       #############################
###########################################################################
###########################################################################
  
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
         "signal1"=dosignal1(req$waitvarname),
         "fa"=dofa(req,con),
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

dolock <- function(lockname,cn) {
   if (length(locks[[lockname]]) == 0)  {  # lock is unlocked
      locks[[lockname]] <<- list(cn)
      srvrwriteobj("resume",cn)
   } else {  # lock is locked, so join the queue
      lng <- length(locks[[lockname]])
      locks[[lockname]][[lng+1]] <<- cn
   }
}

dounlock <- function(lockname) {
   # current connection done with lock, so remove 
   locks[[lockname]] <<- locks[[lockname]][-1]  
   if (length(locks[[lockname]]) > 0)  # others are waiting for lock 
      srvrwriteobj("resume",locks[[lockname]][[1]])
}

dowait <- function(waitvarname,cn) {
   # join group of waiting connections
   if (length(waitvars[[waitvarname]]) == 0)  {  
      waitvars[[waitvarname]] <<- list(cn)
   } else {
      waitvars[[waitvarname]] <<- c(waitvars[[waitvarname]],list(cn))
   }
}

# wakes all processes waiting for waitvarname
dosignal <- function(waitvarname) {
   # signal all who are waiting
   wv <- waitvars[[waitvarname]]
   waitvars[[waitvarname]] <<- NULL
   for (cn in wv) {
      srvrwriteobj("resume",cn)
   }
}

# wakes first process waiting for waitvarname
dosignal1 <- function(waitvarname) {
   # signal first that is waiting
   cn <- waitvars[[waitvarname]][[1]]
   waitvars[[waitvarname]][[1]] <<- NULL
   srvrwriteobj("resume",cn)
}

# fetch-and-add; see fa() in Clnt.R
dofa <- function(rq,cn) {
   oldval <- glbls[[rq$fav]]$val
   glbls[[rq$fav]]$val <<- oldval + rq$inc
   srvrwriteobj(oldval,cn)
}

# client cn quits
quit <- function(cn) {
   remainingclients <<- remainingclients - 1
}

###########################################################################
###########################################################################
################        autolaunch routines       #########################
###########################################################################
###########################################################################

# auto-launcher 

# manages Rdsm from one central window; sets up server and client windows,
# and enables user to send a single command to all clients to run, thus
# saving typing into each one individually

# this code should run on most Linux/Unix systems, and under Cygwin;
# assumes a common file system, either with the clients running on the
# same machine or on a cluster

# USAGE SUMMARY:  

#    start R in one window 

#    call alinit() with the desired number/locations of clients; this
#    creates the client windows, then starts R and load Rdsm (and by
#    default, bigmemory) in them

#    call cmdtoclnts() each time you wish to run any command on all the
#    clients

#    call go() to start the server and the clients

#    example--run 2 clients on the current machine, with a function
#    x(k,n) with source file y.R:

#       alinit(2)  # create clients
#       cmdtoclnts('source("y.R")')  # clients source code
#       go()  # set up server/client connections
#       x(3,100)  # first run
#       x(12,5000)  # second run

# alinit() sets up nclnt clients on the nodes nds; exactly one of nclnt
# and nds must be NULL; if nclnt is NULL, then nclnt = length(nds),
# while if nds is NULL, the nodes are assumed to be "localhost" and
# nclnt specifies that the number of clients; also starts up Rdsm at
# each client, and also bigmemory if requested
alinit <- function(nclnt=NULL,nds=NULL,bigmem=F) {
   if (is.null(nclnt)) {
      nclnt <- length(nds)
   } 
   nclnt <<- nclnt  # make a global copy
   # spawn the R sessions, server first then clients
   rcmd <- "R --no-save -q"
   spawnsrvrcmd <- 
      paste("xterm -e 'screen -S rdsmsrvr -d -RR ",rcmd,"' &")
   system(spawnsrvrcmd)
   clntnames <<- vector(length=nclnt,mode="character")
   for (i in 1:nclnt) {
      clntnames[i] <<- sprintf("rdsmclnt%d",i)
      if (is.null(nds)) {  # clients are at localhost
         xtermcmd <- 
            sprintf("xterm -e 'screen -S %s -d -RR %s' &",clntnames[i],rcmd)
      } else {
         xtermcmd <- 
            sprintf("xterm -e 'screen -S %s -d -RR ssh -x %s %s' &",
               clntnames[i],nds[i],rcmd)
      }
      system(xtermcmd)
      Sys.sleep(0.5)
   }
   loadrdsm(nclnt,bigmem=bigmem)
}

# sends command cmd, SINGLE-quoted, to all clients; double quotes OK if
# no quotes within the command
cmdtoclnts <- function(cmd) {
   for (i in 1:nclnt) {
      system(sprintf("screen -S %s -X stuff '%s\n'",clntnames[i],cmd))
      Sys.sleep(0.1)
   }
}

# sends command cmd, SINGLE-quoted (see above), to server
cmdtosrvr <- function(cmd) {
   system(sprintf("screen -S rdsmsrvr -X stuff '%s\n'",cmd))
}

# sends command cmd, single-quoted, to clients and server
cmdtoall <- function(cmd) {
   cmdtoclnts(cmd)
   cmdtosrvr(cmd)
}

# set up server/client connections
go <- function(ncon=2) {
   srvrcmd <- sprintf('srvr(ncon=%d)',ncon)
   cmdtosrvr(srvrcmd)      
   Sys.sleep(0.5)
   cmdtoclnts('init()')
}

# have everyone load Rdsm, and also bigmemory if requested
loadrdsm <- function(nclnt,bigmem=F) {
   cmdtoall('library(Rdsm)')
   if (bigmem) cmdtoclnts('library(bigmemory)')
}
