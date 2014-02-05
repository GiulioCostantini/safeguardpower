safeguard.r.data<-function(x, y, sig.level= .05, power = .8, conf = .80)
{
  if(length(x) != length(y)) stop("x and y must have the same length")
  r <- cor(x,y)
  n <- length(x)
  safeguard.r(r, n, sig.level = sig.level, power = power, conf = conf)
}