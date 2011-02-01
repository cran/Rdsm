# test wait/signal/signal1

# run with at least 3 clients; hit ctrl-c to quit; in the "all" case,
# each time hit Enter in client 1, the other clients should each
# complete one wait; in the "one" case, only one client should react, in
# rotation, so that with for instance 3 clients, clients 2 and 3 will
# alternate

# argument sigtype, "all" or "one", determines whether signal() or
# signal1() is called
test <- function(sigtype) {
   me <- myinfo$myid
   message("I am ",me)
   barr()
   if (me == 1) {
      repeat {
         readline(prompt="wait at least 1 sec, then hit Enter")
         if (sigtype == "all") signal("ws") else signal1("ws")
      }
   } else {
      repeat {
         Sys.sleep(runif(1))
         message(me," starts a wait")
         wait("ws")
      }
   }
}
