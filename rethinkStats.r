#Code to  follow along rethinking stats with bayes, r.mcelreath
#ch2  set up grid approximation

#define grid
count<-1000
p_grid<-seq(from=0,to=1,length.out=count)
#define prior
prior<-rep(1,count)  #this is basically a uniform prior (I think)
#prior <-ifelse(p_grid<.5,0,1) #  this prior is a .5 assumption
#=======

#build a function that does the above

#prior <-ifelse(p_grid<.5,0,1) #  this prior assumes zero below .5 and 1 above
#prior<-exp(-5*abs(p_grid-.5))  #this prior has a sharp peak

posterior.from.grid<-function(trials, x, n, prior=prior){
  p_grid<-seq(from=0,to=1,length.out=trials)
  prior<-rep(1,trials)  #uniform prior
  #prior<-ifelse(p_grid<=.5,0,1)  #Stepwise prior
  #prior<-ifelse(p_grid<=.5 | p_grid>=.8,0,1)  #Stepwise nuanced prior
  #compute likelihood at each value in grid
  likelihood <- dbinom(x,size=n,prob=p_grid) #likelihood could be some other distribution, this assumes binomial
  #compute product of likelihood and prior
  unstd.posterior <- likelihood * prior  #posterior is proportional to likelihood and prior
  #standardize the posterior so it sums to 1
  posterior<-unstd.posterior/sum(unstd.posterior)  #ways to get event divided by sum of ways
  output<-list(prior,p_grid,posterior,trials)
  return (output)
  }


plot.pfg<-function(x,y,trials){
  plot(x=x,y=y,
       type='b', xlab='probability of water',ylab="posterior probability")
        title <- paste( length(x), "trials")
        mtext(title)
        #mtext(text=c(trials=trials))
       abline(v=posterior[[2]][which.max(posterior[[3]])],col='blue')
       text(x=.5,y=.0010,round(posterior[[2]][which.max(posterior[[3]])],4),col='blue')
       abline(h=posterior[[3]][which.max(posterior[[3]])],col='red')
       text(x=.5,y=.1,round(posterior[[3]][which.max(posterior[[3]])],4),col='red')
}

posterior<-posterior.from.grid(trials=10,x=6,n=9)
plot.pfg(x=posterior[[2]],y=posterior[[3]],trials=posterior[4])

###simple posterior from grid approx
p_grid<-seq(from=0,to=1,length.out=1000)
p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(3,size=3,prob=p_grid)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p_grid,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')


samples<-sample(p_grid,  prob = posterior,size = 1e4,replace = T)
samples<-sample(p,  prob = posterior,size = 1e4,replace = T)


plot(samples,col=adjustcolor('blue',alpha=.3))
library(ggplot2)
df<-data.frame(cbind(samples,posterior))
ggplot(data=df,aes(x=samples,y=posterior,col=posterior))+geom_point()
qplot(samples)

plot(density(samples))

#Ch3
library(rethinking)
dens(samples)
sum(posterior[p<.5])
sum(samples<.5)/1e4
sum(samples>.5 & samples < .75)/1e4
PI(samples,prob=.5)
HPDI(samples,prob=.5)

sum(posterior*abs(.5-p_grid))
loss<-sapply(p_grid,function(d) (sum(posterior*abs(d-p_grid)))) #list of loss values ? d is for difference? This is considered the aboslute loss function which  leads to the median, or you could do the quadratic loss  (d-p)^2, which leads to the posterior mean
loss<-sapply(p_grid,function(d) (sum(posterior*abs((d-p_grid)^2))))

p_grid[which.min(loss)]#this is the posterior median, the parameter value that splits the post such that half of mass is above and below.

dbinom(0:2,size=2,prob = .7)
rbinom(10,size=2,prob = .7)

dummy_w<-rbinom(1e5,size=2,prob = .7)
dw<-data.frame(table(dummy_w)/1e5)

simplehist(dummy_w,xlab='dummy water count')

w<-rbinom(1e4,size=9,prob=.6)
simplehist(w,xlab='dummy wataer count')


#Chapter 3

#simulating predictions from total posterior pg 66, usin samples from above
w<-rbinom(1e4,size = 9, prob = samples)
w<-rbinom(1e4,size = 9, prob = samples[samples >=.6 & samples <=.7])
hist(w)
simplehist(w)
plot(samples)
median(samples)


#chapter ends with interesting ways to test if model fails: counting the number of runs of w (what would we expect if water is .7 covering planet?) and the # of switches from L to W, again, assuming a prob of water coverage.

#ch 4 linear models

pos<-data.frame(replicate(1000,sum(runif(16,-1,1))))
colnames(pos)<-'distance'

ggplot(data=pos,aes(x=1:nrow(pos),y=distance,group=1:nrow(pos)))+geom_line(aes(y=))

  #rcode 4.6
=======
pos<-replicate(1000,sum(runif(16,-1,1)))
prod(1 + runif(12,0,.1))
growth<-replicate(1e3,prod(1 + runif(12,0,.1)))
dens(growth,norm.comp=T)
big<-replicate(1e3,prod(1 + runif(12,0,.5)))
small<-replicate(1e3,prod(1 + runif(12,0,.01)))
par(mfrow=c(1,2))
dens(big,norm.comp=T,main = "big")
dens(small,norm.comp=T,main = "small")

log.big<-replicate(1e3,log(prod(1 + runif(12,0,.5))))
dens(log.big,norm.comp=T,main = "log.big")


w<-6; n<-9;
p_grid<-(seq(0,1,length.out = 100))
posterior<-dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior<-posterior/sum(posterior)
plot (p_grid,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')



#chap 4 modelling
data(Howell1)
d<-Howell1
str(d)
summary(d)

d2<-d[d$age>=18,]
curve(dnorm(x,178,20),from=100,to=250)
abline(v=(2*20)+178,col='red')
abline(v=178-(2*20),col='blue')

sample_mu <- rnorm(1e4,178,20)
sample_sigma <- runif(1e4,0,50)
prior_h<-rnorm(1e4,sample_mu,sample_sigma)
dens(prior_h)
plot(prior_h)




#pg84
plot(sample(sample_mu,100,replace=T),sample(sample_sigma,100,replace=T))



data(Howell1)
d <- Howell1

## R code 4.8
str( d )

## R code 4.9
d$height

## R code 4.10
d2 <- d[ d$age >= 18 , ]

## R code 4.11
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

## R code 4.12
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

## R code 4.13
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

## R code 4.14
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

## R code 4.15
contour_xyz( post$mu , post$sigma , post$prob )

## R code 4.16
image_xyz( post$mu , post$sigma , post$prob )

## R code 4.17
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

## R code 4.18
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

## R code 4.19
dens( sample.mu )
dens( sample.sigma )

## R code 4.20
HPDI( sample.mu )
HPDI( sample.sigma )

## R code 4.21
d3 <- sample( d2$height , size=20 )

## R code 4.22
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

## R code 4.23
dens( sample2.sigma , norm.comp=TRUE )
