
safeguard.r<-function(r, n, sig.level=.05, power=.8, conf=.80)
{
  if (r>=0) alternative="greater" else alternative="less"
  if (conf<.5 | conf>=1) stop ("The parameter conf must be in the interval .5<conf<1")
  rci<-r.con(rho=r, n=n, p=conf, twotailed=F)
  if(sign(rci[1]*rci[2])<0)
  {
    warning("the safeguard r is negative: the safeguard sample size is therefore set to Inf")
    return (c("Lower_r"=rci[sign(rci)!=sign(r)], "N_required"=Inf, "SSR"=Inf))
  }
  r.sg<-sign(rci[1])*min(abs(rci))
  pw<-tryCatch(pwr.r.test(n=NULL, r=r.sg, sig.level=sig.level, power=power, alternative=alternative),
               error={function(err)
               {return(NA)}
               })
  if(all(is.na(pw)) & r.sg>=.5)
  {
    warning("the value of r is so high that it was not possible to compute a precise value of sample size. The sample size was therefore rounded to 2")
    N_required<-2
  }
  if(all(is.na(pw)) & r.sg<.5)
  {
    warning("the safeguard r is too small: the safeguard sample size could not be computed exactly and is therefore set to Inf")
    return (c("Lower_r"=r.sg, "N_required"=Inf, "SSR"=Inf))
  }
  if(!all(is.na(pw))) N_required=ceiling(pw$n)
  SSR=N_required/n
  c("Lower_r"=r.sg, "N_required"=N_required, "SSR"=SSR)
}