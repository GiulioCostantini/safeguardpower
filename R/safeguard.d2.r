# safeguard.d2<-function(m1, m2, pooledsd, n.1, n.2, conf=.80, sig.level=.05, power=.8)
# {
#   d<-abs(m2-m1)/pooledsd
#   safeguard.d(d, n.1, n.2, conf=conf, sig.level=sig.level, power=power)
# }

# old version that required pooled SD instead of computing it
# safeguard.d2<-function(m1, m2, pooledsd, n.1, n.2, conf=.95, sig.level=.05, power=.8)
safeguard.d2<-function(m.1, m.2, sd.1, sd.2, n.1, n.2, sig.level=.05, power=.8, conf=.80)
{
  d <- abs(m.1 - m.2)/(sqrt(((n.1 - 1) * sd.1^2 + (n.2 - 1) * sd.2^2)/(n.1 + n.2 - 2)))
  safeguard.d(d, n.1, n.2, conf=conf, sig.level=sig.level, power=power)
}