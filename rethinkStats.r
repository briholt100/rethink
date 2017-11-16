#Code to  follow along rethinking stats with bayes, r.mcelreath

#ch2  set up grid approximation
#define grid
p_grid<-seq(from=0,to=1,length.out=20)
#define prior
prior<-rep(1,20)  #this is basically a uniform prior (I think)
#prior <-ifelse(p_grid<.5,0,1) #  this prior is a .5 assumption
#prior<-exp(-5*abs(p_grid-.5))  #this prior has a sharp peak

#compute likelihood at each valuein grid
likelihood <- dbinom(6,size=9,prob=p_grid)
#compute product of likelihood and prior
unstd.posterior <- likelihood * prior
#standardize the posterior so it sums to 1
posterior<-unstd.posterior/sum(unstd.posterior)

plot (p_grid,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')
mtext('20 points')


p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(6,size=9,prob=p)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')
mtext('20 points')

samples<-sample(p,prob = posterior,size = 1e4,replace = T)
plot(samples,)
library(rethinking)
