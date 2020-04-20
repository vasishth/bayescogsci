## to-do: include venn diagram?


## to-do: do we need to spell this out more?


## --------------------------------------------------------------------------
rbinom(10,n=20,prob=0.5)


## ---- binomplot,echo=FALSE,fig.cap="Probability mass functions of a binomial distribution assuming 10 trials, with 50%, 10%, and 90% probability of success."----
## to-do remove warnings
#library(tibble)
#ggplot(tibble(x=as.integer(0:10), aes(x)) +
#    stat_function(fun=dbinom, geom = "point", 
#                  args = list(size=10, prob=0.5))
op<-par(mfrow=c(1,3),pty="s")
plot(0:10,dbinom(0:10,size=10,prob=0.5),
     xlab="possible outcomes",
     ylab="probability",
     main=expression(paste(theta,"=0.5",sep="")))
plot(0:10,dbinom(0:10,size=10,prob=0.1),xlab="possible outcomes",
     ylab="probability",main=expression(paste(theta,"=0.1",sep="")))
plot(0:10,dbinom(0:10,size=10,prob=0.9),xlab="possible outcomes",
     ylab="probability",
     main=expression(paste(theta,"=0.9",sep="")))


## to-do bar or line graphs above, instead of points


## ---- binomlik,echo=FALSE,fig.cap="The likelihood function for 7 successes out of 10."----
theta<-seq(0,1,by=0.001)
plot(theta,dbinom(7,size=10,prob=theta),
     xlab=expression("theta"),ylab="probability",
     main="Likelihood function",type="l")
abline(v=0.7)
text(0.7,0.1,"Max. value at: \n 0.7")


## DS comment: do we want to show the code for computing all likelihood values? (maybe this comes later?)


## ----dnormexample1,echo=TRUE-----------------------------------------------
dbinom(5,size=10,prob=0.5)


## --------------------------------------------------------------------------
dbinom(5,size=10,prob=c(0.1,0.9))


## ----cdfbinom1,echo=TRUE---------------------------------------------------
## the cumulative probability of obtaining
## 0, 1, or 2 successes out of 10,
## with theta=0.5:
dbinom(0,size=10,prob=0.5)+dbinom(1,size=10,prob=0.5)+
  dbinom(2,size=10,prob=0.5)


## --------------------------------------------------------------------------
sum(dbinom(0:2,size=10,prob=0.5))


## ----pbinomexample1,echo=TRUE----------------------------------------------
pbinom(2,size=10,prob=0.5,lower.tail=TRUE)


## ----pbinomexample2,echo=TRUE----------------------------------------------
pbinom(2,size=10,prob=0.5,lower.tail=FALSE)


## ---- binomcdf,echo=FALSE,fig.cap="The cumulative distribution function for a Binomial distribution assuming 10 trials, with 50% probability of success."----
#op<-par(mfrow=c(1,2),pty="s")
k <- c(1:10)
#plot(k,dbinom(k,size=10,prob=.50),type="h",
#     main="Binomial(n=10,prob=.50)")

fx <- function(x) {
  if(x >= 0 && x < 3) {
    res <-  pbinom(2,size=10,prob=0.50)
  } else if(x >=3 && x < 5) {
    res <- pbinom(4,size=10,prob=0.50)-pbinom(2,size=10,prob=0.50)
  } else if(x >= 5 && x < 6) {
    res <-  pbinom(5,size=10,prob=0.50)-pbinom(4,size=10,prob=0.50)
  } else if(x >= 7 && x < 10) {
    res <-  pbinom(9,size=10,prob=0.50)-pbinom(6,size=10,prob=0.50)
  } else {
    res <- 0
  }

  return(res)
}

fx   <- Vectorize(fx)
grid <- 0:10
p    <- fx(grid)
cdf  <- cumsum(p)

plot(grid, cdf, type = 'p', ylim = c(0, 1), col = 'steelblue',
     xlab = 'k', ylab = expression(F(k)), pch = 19, las = 1)
segments(x0 = grid, x1 = grid + 1, y0 = cdf)
segments(x0 = grid + 1, y0 = c(cdf[-1], 1), y1 = cdf, lty = 2)

#plot(0:10,pbinom(0:10,size=10,prob=0.5),
#     xlab="Possible outcomes k",
#     ylab="Prob. of k or less successes",
#     main="Cumulative distribution function")


## ---- eval=FALSE,binominvcdf,echo=FALSE,fig.cap="The inverse cumulative distribution function for a binomial distribution assuming 10 trials, with 50% probability of success."----
## 
## plot(0:10~pbinom(0:10,size=10,prob=0.5),
##      ylab="Possible outcomes k",
##      xlab="Prob. of k or less successes",
##      main="Cumulative distribution function")


## ----qbinomexample,echo=TRUE-----------------------------------------------
qbinom(0.37,size=10,prob=0.5)


## to-do: explain why qbinom(0.77 gives 5 as an answer and not 4)


## DS comment: maybe it’s good to include an additional Figure for the inverse CDF and an example


## ----rbinomexample,echo=TRUE-----------------------------------------------
rbinom(1,size=10,prob=0.5)


## to-do: introduce Bernoulli here and link it with the code below


## ----rbinomexamplemean,echo=TRUE-------------------------------------------
y<-rbinom(10,size=1,prob=0.5)
mean(y)*10 ; sum(y)


## ---- normdistrn,echo=FALSE,fig.cap="The PDF, CDF, and inverse CDF for the $Normal(\\mu=500,\\sigma=100)$."----
op<-par(mfrow=c(1,3),pty="s")
plot(function(y) dnorm(y,mean=500,sd=100), 200, 900,
      main = "PDF of Y ~ Normal(500,100)",
              ylab="density",xlab="y")
plot(function(y) pnorm(y,mean=500,sd=100), 200, 900,
      main = "CDF of Y ~ Normal(500,100)",
              ylab="probability",xlab="y")
plot(function(y) qnorm(y,mean=500,sd=100), 0, 1,
      main = "Inverse CDF of Y ~ Normal(500,100)",
              ylab="y",xlab="probability")


## to-do: Maybe this is the place to mention some interesting properties like:

## Normal(mu, sigma) = mu + Normal(0,1) * sigma

## (We'll use this property a lot later when we code in Stan)


## ----pnormexample----------------------------------------------------------
pnorm(700,mean=500,sd=100)-pnorm(200,mean=500,sd=100)


## to-do: add figure illustrating the above


## ----qnormexample----------------------------------------------------------
qnorm(0.975,mean=500,sd=100)


## ----rnormexample----------------------------------------------------------
y<-rnorm(10,mean=500,sd=100)
mean(y);var(y)


## --------------------------------------------------------------------------
## density:
dnorm(1,mean=0,sd=1)


## --------------------------------------------------------------------------
pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1)


## --------------------------------------------------------------------------
dbinom(2,size=10,prob=0.5)
pbinom(2,size=10,prob=0.5)-pbinom(1,size=10,prob=0.5)


## --------------------------------------------------------------------------
BinLik<-function(theta){
  choose(10,8)*theta^8 * (1-theta)^2
}
integrate(BinLik,lower=0,upper=1)$value


## to-do: add summary


## ----echo=FALSE------------------------------------------------------------
## DATA GENERATION
mean.val<-round(rnorm(1,mean=100,sd=100),digits=0)
sd.val<-round(rnorm(1,mean=100,sd=1.5),digits=0)

q1<-round(rnorm(1,mean=100,sd=100)+50,digits=0)
q2<-round(rnorm(1,mean=100,sd=100)-100,digits=0)

if(q1>q2){
sol<-pnorm(q1,mean=abs(mean.val),sd=abs(sd.val))-pnorm(q2,mean=abs(mean.val),sd=abs(sd.val))
} else {
  sol<-pnorm(q2,mean=abs(mean.val),sd=abs(sd.val))-pnorm(q1,mean=abs(mean.val),sd=abs(sd.val))
}


## ----echo=FALSE------------------------------------------------------------
mu<-round(rnorm(1,mean=51,sd=2),digits=0)
sigma<-round(runif(1,min=2,max=4),digits=0)

q<-round(runif(1,min=40,max=50))
q1<-round(runif(1,min=51,max=60))


p1<-round(pnorm(q,mean=mu,sd=sigma),digits=3)
p2<-round(1-pnorm(q,mean=mu,sd=sigma),digits=3)
p3<-round(1-pnorm(q1,mean=mu,sd=sigma),digits=3)


## ----echo=FALSE------------------------------------------------------------
mu<-round(runif(1,min=45,max=55),digits=0)
sigma<-round(runif(1,min=2,max=10),digits=0)

p1<-round(pnorm(mu-5,mean=mu,sd=sigma),digits=3)
p2<-round(pnorm(mu+3,mean=mu,sd=sigma)-pnorm(mu-3,mean=mu,sd=sigma),digits=3)
p3<-round(1-pnorm(mu+1,mean=mu,sd=sigma),digits=3)


## ----echo=FALSE------------------------------------------------------------
prob1<-round(runif(1,min=0,max=1),digits=2)
if(prob1<=0.5){
 prob2<-round(runif(1,min=0.55,max=0.99),digits=2)
} else {
    prob2<-prob1
    prob1<-round(runif(1,min=0.01,max=0.40),digits=2)}

lower<-round(qnorm(prob1,mean=1,sd=1),digits=3)
upper<-round(qnorm(prob2,mean=1,sd=1),digits=3)


## ----echo=FALSE------------------------------------------------------------
mu<-round(runif(1,min=50,max=60),digits=3)
sigma<-round(runif(1,min=0.5,max=1.5),digits=3)
probs<-c(.80,.85,.90,.95,.99)
prob<-sample(probs,1)
tailprob<-(1-prob)/2
q1<-round(qnorm(tailprob,mean=mu,sd=sigma),digits=3)
q2<-round(qnorm(tailprob,mean=mu,sd=sigma,lower.tail=FALSE),digits=3)


## ----echo=FALSE------------------------------------------------------------
n<-round(runif(1,min=140,max=170))
mu<-round(runif(1,min=120,max=180),0)
sigma<-round(runif(1,min=30,max=80),0)
x<-rnorm(n,mean=mu,sd=sigma)
sample.sd<-round(sd(x),3)
sample.mean<-round(mean(x),digits=3)
estimated.se<- round(sample.sd/sqrt(n),3)
crit.t<-abs(round(qt(0.025,df=n-1),3))
lower<-round(sample.mean-crit.t*estimated.se,digits=3)
upper<-round(sample.mean+crit.t*estimated.se,digits=3)


## ----echo=FALSE------------------------------------------------------------
x<-round(rnorm(1,mean=12,sd=5),3)
mn<-seq(9,12,by=1)
ans1<-round(dnorm(x,mean=mn[4],sd=5),3)
ans2<-round(dnorm(x,mean=mn[3],sd=5),3)
ans3<-round(dnorm(x,mean=mn[2],sd=5),3)
ans4<-round(dnorm(x,mean=mn[1],sd=5),3)


## ----echo=FALSE------------------------------------------------------------
x<-round(rnorm(10,mean=500,sd=10),0)
mn<-mean(x)
loglik<-round(sum(dnorm(x,mean=mn,sd=5,log=TRUE)),3)
loglik2<-round(sum(dnorm(x,mean=mn-2,sd=5,log=TRUE)),3)


## --------------------------------------------------------------------------
x


## --------------------------------------------------------------------------
dnorm(5,mean=10,sd=2)*dnorm(10,mean=10,sd=2)


## --------------------------------------------------------------------------
log(2*3)
log(2) + log(3)


## --------------------------------------------------------------------------
dnorm(5,mean=10,sd=2,log=TRUE)+dnorm(10,mean=10,sd=2,log=TRUE)


## --------------------------------------------------------------------------
sum(dnorm(c(5,10),mean=10,sd=2,log=TRUE))

