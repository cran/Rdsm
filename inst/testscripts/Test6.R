# test wait/signal

test <- function() {
   me <- myinfo$myid
   message("I am ",me)
   barr()
   if (me == 1) {
      repeat {
         readline(prompt="wait a bit, then hit Enter")
         signal("ws")
      }
   } else {
      repeat {
         Sys.sleep(5*runif(1))
         message(me," starts a wait")
         wait("ws")
      }
   }
}

# run with at least 3 clients; hit ctrl-c to quit
