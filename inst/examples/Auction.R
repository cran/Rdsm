# auction manager; each node runs this program on 2 processes, one for
# monitoring bids and the other for placing them

# fancier versions could be constructed, e.g. with a graphical display,
# bidder names, etc.

# test by calling auction(); best with at least 3 bidders, i.e. at least
# 6 clients; run for a while, then have bidders withdraw

# author:  N. Matloff

# arguments:  none
# return value:  none
# locks:
#    bidlock: lock guarding the variable latestbid
auction <- function() {
   if (myinfo$nclnt %% 2 != 0)
      stop("need even number of clients")
   cnewdsm("latestbid","dsmv","integer",0)
   cnewdsm("nbidders","dsmv","integer",myinfo$nclnt/2)
   barr()
   if (myinfo$myid%%2 == 1) {  # monitor bid
      print("no bids yet")
      oldbid <- latestbid[1]
      oldnbidders <- nbidders[1]
      repeat {
         wait("change")
         if (latestbid[1] != oldbid) {
            oldbid <- latestbid[1]
            message("latest bid is ",oldbid)
         }
         newnbidders <- nbidders[1]
         if (newnbidders <= 1) {
            message("auction over")
            dsmexit()
            return()
         }
         if (newnbidders != oldnbidders) {
            oldnbidders <- newnbidders
            message("there are ",newnbidders," bidders remaining")
         }
      }
   } else {  # place bid
      repeat {
         bid <- readline("place bid (0 to drop out: ")
         bid <- as.integer(bid)
         if (bid == 0) {
            fa("nbidders",-1)
            signal("change")
            dsmexit()
            return()
         }
         lock("bidlock")
         # make sure someone else hasn't bid more after readline() call
         newbid <- latestbid[1] 
         if (bid <= newbid) {
            message("your bid was canceled")
         } else {
            latestbid[1] <- bid
            signal("change")
         }
         unlock("bidlock")
      }
   }
}
