#Code to  follow along rethinking stats with bayes, r.mcelreath
#ch2  set up grid approximation

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
p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(6,size=9,prob=p)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')


samples<-sample(p,  prob = posterior,size = 1e4,replace = T)

plot(samples,col=adjustcolor('blue',alpha=.3))
library(ggplot2)
qplot(samples)
plot(density(samples))
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






#Chapter 3

#simulating predictions from total posterior pg 66, usin samples from above

w<-rbinom(1e4,size = 9, prob = samples[samples <.4 & samples >.3])
hist(w)
plot(samples)
median(samples)
