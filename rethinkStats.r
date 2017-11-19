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


samples<-sample(p,  prob = posterior,size = 1e4,replace = T)
plot(samples,col=adjustcolor('blue',alpha=.5))
library(ggplot2)
qplot(samples)
library(rethinking)
dens(samples)
sum(posterior[p<.5])
sum(samples<.5)/1e4
sum(samples>.5 & samples < .75)/1e4


#chapter 2 excercises
#easy
w<-c(.3,1);
pulls<-c(.5,.5)
w*pulls/sum(w*pulls)


ways<-c(0,1,2)
ways/sum(ways)

ways<-c(0,1,4)
ways/sum(ways)

#medium
ways<-c(0,1,2)
pull<-c(3,2,1)
df<-data.frame(cbind(ways,pull))

new.ways<-ways*pull

df<-data.frame(cbind(df,new.ways,new.ways/sum(new.ways)))

colnames(df)<-(c("prior ways",'weighted',"new ways to pull","final prob"))

p_grid<-seq(from=0,to=1,length.out=20 )
print("here is the pgrid")
p_grid

prior <-rep(1,20)  #uniform prior assumption
prior <-ifelse(p_grid<.5,0,1)  #stepwise
prior

trials<-matrix(data=c(3,3,3,4,5,7),ncol=2,nrow=3, byrow=T)
par(mfrow=c(2,2))
for(i in 1:nrow(trials)){
  cat ("binomial demo ",i)
  print(dbinom(trials[i,1], trials[i,2],prob=p_grid))
  plot(dbinom(trials[i,1], trials[i,2],prob=p_grid),type='b')
}

likelihood<-dbinom(6,9,prob=p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
df1<-cbind(p_grid,round(posterior,10))
plot(df1,type='b')

#Hard

#panda A gives twins 10%
#panda B gives twins 20%
#each panda type 50% likely

#witness a new panda give twins.  What's the probability the next birth is twins?

#twins similar to water, singles to land.
