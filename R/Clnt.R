# Rdsm client/server code

# author:  N. Matloff

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
#
# one client, say client 1, sends to server, with non-NULL val, NULL
# size, and then registers this variable at the server; the other clients
# have NULL val, non-NULL size, and register the variable only locally
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

# read operation on dsml object l; receives values from server
"[.dsml" <- function(l,subs=NULL) {
   subs <- replacenullneg(subs,l$size)
   return(recvvml(l,subs))
}

# write operation on dsml object l; sends server values to be written 
# R requires that last arg for [<- generics be named "value"
"[<-.dsml" <- function(l,subs=NULL,value) {
   subs <- replacenullneg(subs,l$size)
   sendvml(l,value,"put",subs)
   # need to restore this variable in client listing of app globals, 
   # since the return value of this function (and other put ops) will 
   # be reassigned by R to l's entry 
   return(l)
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

# see notes on locks at top of this file
lock <- function(lockname) {
   tosrvr <- list(reqtype="lock",lockname=lockname)
   writeobj(tosrvr)
   # wait for resume signal
   readobj()
   return()
}

# see notes on locks at top of this file
unlock <- function(lockname) {
   tosrvr <- list(reqtype="unlock",lockname=lockname)
   writeobj(tosrvr)
}

# see notes on wait at top of this file
wait <- function(waitvarname) {
   tosrvr <- list(reqtype="wait",waitvarname=waitvarname)
   writeobj(tosrvr)
   # wait for resume signal
   readobj()
   return()
}

# see notes on wait at top of this file
signal <- function(waitvarname) {
   tosrvr <- list(reqtype="signal",waitvarname=waitvarname)
   writeobj(tosrvr)
}

# send new function to server, for use in Remote Procedure Calls
newf <- function(ftnname,ftn) {
   tosrvr <- list(ftnname=ftnname,ftn=ftn,reqtype="newf")
   writeobj(tosrvr)  
}

# Remote Procedure Call; the function must have already been sent to the
# server, using newf() above; arglist is list of zero or more arguments;
# an argument can consist of either an expression not involving any Rdsm
# variables, in which case it is evaluated at the client side, or a
# single quoted Rdsm variable name
rpc <- function(ftnname,arglist) {
   tosrvr <- list(ftnname=ftnname,arglist=arglist,reqtype="rpc")
   writeobj(tosrvr)  
   rslt <- readobj()
}

# close this client's connection
closecon <- function() close(myinfo$con)

# quit
dsmexit <- function() {
   tosrvr <- list(reqtype="quit")
   writeobj(tosrvr)
}
  
