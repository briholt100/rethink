#Code to  follow along rethinking stats with bayes, r.mcelreath
library(ggplot2)
#ch2  set up grid approximation
#define grid
count<-1000
p_grid<-seq(from=0,to=1,length.out=count)
#define prior
prior<-rep(1,count)  #this is basically a uniform prior (I think)
#prior <-ifelse(p_grid<.5,0,1) #  this prior is a .5 assumption
#prior<-exp(-5*abs(p_grid-.5))  #this prior has a sharp peak

#compute likelihood at each valuein grid
likelihood <- dbinom(6,size=9,prob=p_grid)
#compute product of likelihood and prior
unstd.posterior <- likelihood * prior
#standardize the posterior so it sums to 1
posterior<-unstd.posterior/sum(unstd.posterior)

plot (x=p_grid,y=posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob',main='counts')
mtext(text=c(count))
p_grid[which.max(p_grid)]
abline(v=p_grid[which.max(posterior)],col='blue')
text(x=.5,y=.0010,round(p_grid[which.max(posterior)],4),col='blue')

p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(3,size=3,prob=p)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')


samples<-sample(p_grid,  prob = posterior,size = 1e4,replace = T)
plot(samples,col=adjustcolor('blue',alpha=.3))
library(ggplot2)
qplot(samples)
library(rethinking)
dens(samples)
sum(posterior[p<.5])
sum(samples<.5)/1e4
sum(samples>.5 & samples < .75)/1e4
PI(samples,prob=.5)
HPDI(samples,prob=.5)

dbinom(0:2,size=2,prob = .7)
rbinom(10,size=2,prob = .7)

dummy_w<-rbinom(1e5,size=9,prob = .7)
table(dummy_w)/1e5

simplehist(dummy_w,xlab='dummy wataer count')

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



#ch 4 linear models

pos<-replicate(1000,sum(runif(16,-1,1)))
#rcode 4.6
w<-6; n<-9;
p_grid<-(seq(0,1,length.out = 100))
posterior<-dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior<-posterior/sum(posterior)


data(Howell1)
d<-Howell1
str(d)
