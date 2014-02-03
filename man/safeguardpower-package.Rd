\name{safeguardpower-package}
\alias{safeguardpower-package}
\alias{safeguardpower}
\docType{package}
\title{
Safeguard Power Analysis
}
\description{
Safeguard power analysis uses the uncertainty in the estimate of the effect size
to achieve a guaranteed likelihood of correctly identifying the population effect size.
This in turn allows for a calculation of the sample size for a replication study
ensuring that the study is not underpowered. 
}
\details{
\tabular{ll}{
Package: \tab safeguardpower\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-12-19\cr
License: \tab GPL (>=2)\cr
}
~~ An overview of how to use the package, ~~
~~ including the most important functions ~~
}
\author{
Giulio Costantini, Marco Perugini, Marcello Gallucci

Maintainer: Giulio Costantini <giulio.costantini@unimib.it>
~~ The author and/or maintainer of the package ~~
}
\references{
Perugini, M., Gallucci, M., Costantini, G. (in preparation)
Safeguard Power as a Protection Against Imprecise Power Estimates
}
\keyword{power analysis}
\keyword{safeguard}
\keyword{replicability}

\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
safeguard.d(d=.8, n.1=25, n.2=25, sig.level=.05, power=.8, conf=0.975)
safeguard.r.CI(r=.4, n=30, nrep=100)
}
