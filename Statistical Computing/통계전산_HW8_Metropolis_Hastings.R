library(tidyverse)


set.seed(0)
N=10000
samples <-c()
alpha <-c()
acceptance<-c()
samples[1] <-0
Transition.kernel=function(x,y,z) {T=z*dnorm(x,mean=y-1.5,sd=1)+(1-z)*dnorm(x,mean=y+1.5,sd=1)
return(T)
}
target.ft=function(x){T=1/sqrt(8*x^2+1)*exp(-0.5*(x^2-8*x-16/(8*x^2+1)))
return(T)
}
for(i in 1:N){
  z1<-rbinom(n=1,size=1,prob=0.6)
  z2<-rbinom(n=1,size=1,prob=0.6)
  candidate = z1*rnorm(1,mean=samples[i]-1.5,sd=1)+(1-z1)*rnorm(1,mean=samples[i]+1.5,sd=1)
  Q3.alpha[i] =
    (target.ft(candidate)/Transition.kernel(candidate,samples[i],z1))/(target.ft(samples[i])/Transition.kernel(samples[i],
                                                                                                               candidate,z2))
  p = min(Q3.alpha[i],1)
  samples[i+1] = sample(c(samples[i],candidate),size=1,prob=c(1-p,p))
  ifelse(samples[i+1]==samples[i],acceptance[i]<-0,acceptance[i]<-1)
}

# acceptance rate
(sum(acceptance) / N)*100
sum(acceptance)
# Time-series and Autocorrelation plot
par(mfrow=c(2,1),mar=c(3,4,1,2))
ts.plot(samples)
acf(samples,main=" ")


par(mfrow=c(1,1))
hist=hist(samples,breaks=50,main="")
lines(seq(-1,8,0.01),target.ft(seq(-1,8,0.01)),lty=2)