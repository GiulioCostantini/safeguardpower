safeguard.d.data<-function(x, y, sig.level=.05, power=.8, conf=.80)
{
  n.1<-length(x)
  n.2<-length(y)
  m.1<-mean(x)
  m.2<-mean(y)
  sd.1<-sd(x)
  sd.2<-sd(y)
  safeguard.d2(m.1, m.2, sd.1, sd.2, n.1, n.2, sig.level=sig.level, power=power, conf=conf)  
}

