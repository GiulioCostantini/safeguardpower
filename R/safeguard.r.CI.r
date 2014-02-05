safeguard.r.CI <- function(r, n, nrep, sig.level = .05, power = .8)
{ 
  tol = 1e-06
  if (r >= 0) alternative = "greater" else alternative = "less"
  CImin <- .5
  CImax <- 1
  ntry = -1
  if(nrep < 2)
  {
    stop("Nrep is too small")
  }
  
  # the nrep must be at least as high as for a standard power analysis
  nmin <- tryCatch((ceiling(pwr.r.test(n = NULL, r = r, sig.level = sig.level, power = power, alternative = alternative)$n)),
                 error = {function(err) {return(NA)}})
  if(is.na(nmin) & r < .5)
  {
    warning ("The effect size d is too small to perform even a standard power analysis")
    return(c("CI" = .5))
  }
  if(is.na(nmin) & r >= .5)
  {
    nmin <- 2
  }
  
  if(nrep < nmin)
  {
    warning("nrep is too small and it does not meet the criteria of a standard power analysis. A value CI = .5 is returned.")
    return(c("CI" = .5))
  }
  
  while(ntry != nrep & CImax != CImin)
  {
    if((CImax-.5) <= tol)
    {
      warning("nrep is too small and it was not possible to determine a precise value of CI. The value is thus rounded to .5.")
      return(c("CI" = .5))
    }
    if((1-CImin) <= tol)
    {
      warning("nrep is too high that it was not possible to determine a precise value of CI. The value is thus rounded to 1")
      return(c("CI" = 1))
    }
    if(ntry == Inf)
    {
      if(safeguard.r(r, n = n, conf = CImin, sig.level = sig.level, power = power)["N_required"] == Inf)
      {
        warning("nrep is too high and it was not possible to determine a precise value of CI. The value is thus rounded to 1.")
        return(c("CI" = 1))
      }
    }
    
    CI <- mean(c(CImin, CImax))
    ntry <- suppressWarnings(safeguard.r(r, n = n, conf = CI, sig.level = sig.level, power = power)["N_required"])
    if(ntry<nrep) CImin <- CI
    if(ntry>nrep) CImax <- CI
  }
  
  # an adjustment to find the largest CI that fulfills the conditions
  CImin <- CI
  while((CImax-CImin) >= tol)
  {
    CInew <- (CImax+CImin)/2
    nnew <- suppressWarnings(safeguard.r(r, n = n, conf = CInew, sig.level = sig.level, power = power)["N_required"])
    if (nnew == nrep) CImin <- CInew
    if (nnew != nrep) CImax <- CInew
  }
  CI <- CImin
  names(CI) <- "CI"
  CI
}