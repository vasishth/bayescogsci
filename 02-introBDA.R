## --------------------------------------------------------------------------
## Define the likelihood function:
LikFun<-function(theta){
  choose(10,8)*theta^8*(1-theta)^(10-8)
}
## compute the area under the curve:
integrate(LikFun,lower=0,upper=1)$value


## ----betas2, echo=FALSE, out.width='48%', fig.show = "hold", fig.cap = "Examples of Beta distributions with different parameters.", tidy = FALSE----
ab <- tibble(a = c(seq(1,10,3), 
                   seq(10,1,-3)), 
             b = c(seq(1,10,3),seq(1,10,3)))
pwalk(ab, function(a,b)
    print(ggplot(data= tibble(theta= c(0,1)), aes(theta)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b))+
    ylab("density") +
    ggtitle(paste0("Beta distribution with a = ",a,", b = ",b))
    +theme_bw())
    )


## --------------------------------------------------------------------------
qbeta(c(0.025,0.975),shape1=4,shape2=4)


## --------------------------------------------------------------------------
qbeta(c(0.025,0.975),shape1=10,shape2=10)


## to-do: introduce the idea of an unnormalized posterior here? see other suggestion elsewhere.


## --------------------------------------------------------------------------
PostFun<-function(theta){
  theta^11 * (1-theta)^5
}
(AUC<-integrate(PostFun,lower=0,upper=1)$value)


## --------------------------------------------------------------------------
PostFun<-function(theta){
  theta^11 * (1-theta)^5/AUC
}
round(integrate(PostFun,lower=0,upper=1)$value,2)


## ----postbeta-viz,echo=FALSE,fig.cap = "The likelihood, prior, and posterior in the Beta-Binomial example."----
## Likelihood
k <- 8
n <- 10
## Prior
a <- 4
b <- 4
ggplot(data= tibble(theta= c(0,1)), aes(theta)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b), 
                  aes(color = "Prior")) + ylab("density") +
    stat_function(fun = dbeta,
              args = list(shape1 = k+a, shape2 = n-k+b), aes(color = "Posterior"))+
     stat_function(fun = dbinom,
                  args = list(x = k, size = n), aes(color = "Likelihood"))+
  theme_bw()


## --------------------------------------------------------------------------
qbeta(c(0.025,0.975),shape1=12,shape2=6)


## to-do: put in a shiny app that varies the a,b parameters and the amount of data, to show how the posterior is influenced by the data and the prior under different scenarios.


## ----shinybetabinomial-----------------------------------------------------
knitr::include_app("https://vasishth.shinyapps.io/AppTypeIPower", 
  height = "500px")


## to-do: check that we do.


## to-do: add summary


## --------------------------------------------------------------------------
## data:
k<-rbinom(n=1,size=10,prob=0.5)
k


## ---- eval=FALSE,fig.cap="\\label{fig1}The Gamma prior for the parameter theta."----
## x<-0:200
## plot(x,dgamma(x,10000/225,100/225),type="l",lty=1,
##      main="Gamma prior",ylab="density",
##      cex.lab=2,cex.main=2,cex.axis=2)


## --------------------------------------------------------------------------
### load data:
data<-c(115,97,79,131)

a.star<-function(a,data){
  return(a+sum(data))
}

b.star<-function(b,n){
  return(b+n)
}

new.a<-a.star(10000/225,data)
new.b<-b.star(100/225,length(data))

### post. mean
(post.mean<-new.a/new.b) 
### post. var:
(post.var<-new.a/(new.b^2)) 


## --------------------------------------------------------------------------
new.data<-c(200)


## --------------------------------------------------------------------------
new.a.2<-a.star(new.a,new.data)
new.b.2<-b.star(new.b,length(new.data))

### new mean
(new.post.mean<-new.a.2/new.b.2)
### new var:
(new.post.var<-new.a.2/(new.b.2^2))

