# AutoLaunch.R, auto-launcher for Rdsm

# runs Rdsm from one central window; sets up server and client windows,
# and enables user to send a command to all clients to run, thus saving
# typing into each one individually

# USAGE:  
#
#    start R in one window 
#
#    call alinit() with the desired number/locations of clients; this 
#    will create the client windows, and start R in them
#
#    call loadrdsm() to load Rdsm at the all the clients and the server
#
#    call cmdtoclnts() each time you wish to run something on the
#    clients; it will send the command to all of them
#
#    call alquit() to quit; the server and client windows will be
#    deleted

# AutoLaunch.R uses the "screen" program, included in most Linux systems
# and available on Windows via Cygwin

# assumes a common file system, either with the clients running on the
# same machine or on a cluster

# alinit() sets up AutoLaunch, with nclnt clients on the nodes nds;
# exactly one of nclnt and nds must be NULL; if nclnt is NULL, then
# nclnt = length(nds), while if nds is NULL, the nodes are assumed to be
# "localhost" and nclnt specifies their number the server will be on
# localhost
alinit <- function(nclnt=NULL,nds=NULL) {
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
   }
   Sys.sleep(0.5)
   cmdtosrvr('print("server")')
}

alinits <- function(nclnt=NULL,nds=NULL) {
   alinit(nclnt,nds)
   Sys.sleep(2)
   setdir()
}

setldir <- function(libdir) {
   lbd <- sprintf("\042%s\042",libdir) 
   lpcmd <- sprintf(".libPaths(%s)",lbd)
   cmdtoall(lpcmd)
}

# put clients in the directory cdir; if NULL, then put them in the
# current directory of the R process invoked by this script
setdir <- function(cdir=NULL) {
   if (is.null(cdir)) cdir <- getwd() 
   setwdcmd1 <- sprintf('setwd("%s")',cdir)
   for (i in 1:nclnt) {
      setwdcmd2 <- 
         paste("screen -S",clntnames[i]," -X stuff '",setwdcmd1,"\n'")
      system(setwdcmd2)
   }
}

# sends command cmd, SINGLE-quoted, to all clients; double quotes OK if
# no quotes within the command
cmdtoclnts <- function(cmd) {
   for (i in 1:nclnt) {
      system(sprintf("screen -S %s -X stuff '%s\n'",clntnames[i],cmd))
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

# have everyone load Rdsm
loadrdsm <- function() {
   cmdtoall("library(Rdsm)")
}

# server, clients exit
alquit <- function() {
   cmdtoall("quit()")
}
