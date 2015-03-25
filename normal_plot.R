require(plyr)
ms <- data.frame(mean = rep(75, 4), sd = c(5, 10, 15, 20))
x <- mdply(ms, rnorm, n=200)
x <- data.frame(t(matrix(x[ , -c(1,2)])))
mean=70; sd=15
lb=60; ub=80

x <- seq(-4,4,length=100)*sd + mean
x <- floor(x)
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="Score", ylab="",
  main="Normal Distribution", axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="gray")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
   signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(20, 100, 10), pos=0)















