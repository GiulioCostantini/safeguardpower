SSR.d<-function(d, SSR=1, conf=.80, sig.level=.05, power=.8, nmin=4, nmax=1e+6)
{
  tol=1e-7
  init_nmax<-nmax
  while(abs(nmin-nmax)>tol)
  {
    n<-mean(c(nmin,nmax))
    sg<-suppressWarnings(safeguard.d(d, n.1=n/2, n.2=n/2, conf=conf, sig.level=sig.level, power=power))
    SSR2<-sg["SSR"]
    if (SSR2<SSR) nmax<-n
    if (SSR2>SSR) nmin<-n
  }
  n<-ceiling(round(n)/2)*2
  sg<-safeguard.d(d, n.1=n/2, n.2=n/2, conf=conf, sig.level=sig.level, power=power)
  SSR2<-sg["SSR"]
  if(SSR2>SSR)
  {
    n<-n+2
    sg<-safeguard.d(d, n.1=n/2, n.2=n/2, conf=conf, sig.level=sig.level, power=power)
    SSR2<-sg["SSR"]
  }
  if(n>=init_nmax) warning("the estimation might be inaccurate. Try to increase the parameter nmax.")
  c("N"=n, "SSR"=as.numeric(SSR2))
}