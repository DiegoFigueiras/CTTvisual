
eq <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(a*(x-b))))))}

a <- 1
b <- -.5
c <- 0

curve(eq, from=-4, to=4, lwd = 3)          ## lwd changes figure weight

a <- 3
b <- .5
curve(eq, col="red", add=TRUE, lwd=3)


g <- abs(mean(pi)-.5)
