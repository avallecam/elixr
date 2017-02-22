rm(list = ls())

## TRIAL of CORRAN et al. eluted samples
x <- seq(0,2.5,.25)
newB <- function(r) { (r*max(x))/((max(x)/x)+r-1) }
plot(x,newB(0.5), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")
segments(0,0,2.5,2.5, lty=2)

newA <- function(r,m) { (r*max(x))/((max(x)/m)+r-1) }
m <- seq(0,4,.25)
plot(m,newA(0.5,m), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")
plot(x,newA(0.5,x), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")