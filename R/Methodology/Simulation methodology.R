require(MASS)
set.seed(51)

ME <-vector()
for (n in 1:500000){
  random<-rnorm(1:13)
  fitdistrib<-fitdistr(random,"normal")
  ME[n]<-dnorm(-2.5,mean=fitdistrib$estimate["mean"], sd=fitdistrib$estimate["sd"])
}

q1<-quantile(ME,c(0.05,0.95))

p1<-qnorm(q1,mean=0,sd=1)

for (n in 1:500000){
  random<-rnorm(1:13)
  fitdistrib<-fitdistr(random,"normal")
  ME[n]<-dnorm(-1,mean=fitdistrib$estimate["mean"], sd=fitdistrib$estimate["sd"])
}

q2<-quantile(ME,c(0.05,0.95))

p2<-qnorm(q2,mean=0,sd=1)


