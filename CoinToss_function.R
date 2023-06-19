CoinToss <- function(N, seed) {
  set.seed(seed)
  flipsequence <- sample(
    x = c(0, 1),
    prob = c(0.5, 0.5), size = N, replace = TRUE
  )
  r <- cumsum(flipsequence)
  n <- 1:N
  runprop <- r / n
  plot(n, runprop,
    type = "p", log = "x",
    xlim = c(1, N), ylim = c(0.0, 1.0), cex.axis = 1,
    xlab = "Flip Number", ylab = "Proportion of Heads", cex.lab = 1,
    main = "Running Proportion of Heads", cex.main = 1
  )
  lines(c(1, N), c(.5, .5), lty = 5)
  flipletters <- paste(c("T", "H")[flipsequence[1:10] + 1], collapse = "")
  displaystring <- paste("Flip Sequence = ", flipletters, "...", sep = "")
  text(5, .9, displaystring, adj = c(0, 1), cex = 1.3)
  text(N, .3, paste("End Proportion = ", runprop[N]), adj = c(1, 0), cex = 1.3)
}


timestart <- Sys.time()
CoinToss(100000000, 8)
timeend <- Sys.time()
runningtime <- timeend - timestart
print(runningtime)
