
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

#easy

###simple posterior from grid approx
p_grid<-seq(from=0,to=1,length.out=1000)
#p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(6,size=9,prob=p_grid)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p_grid,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')
set.seed(100)
samples<-sample(p_grid,  prob = posterior,size = 1e4,replace = T)

#3e1 how much posterior probabilty lies below p = .2?
sum(posterior[p_grid < 0.2])
#[1] 0.0008560951

#3e2 how much posterior probabilty lies above p = .8?

sum(posterior[p_grid>.8])   #[1] 0.1203449

#3e3 how much posterior probabilty lies between p = .2 and p = .8?

sum(posterior[p_grid>.2 & p_grid<.8])   #[1] 0.878799

#3e4 20% of posterior probability lies below which value of p?

quantile(samples,.2)   #[1] 0.5195195

plot(density(samples))
abline(v=quantile(samples,.2),col='red')

#3e5 20% of posterior probability lies above which value of p?
quantile(samples,.8)    #[1] 0.7567568 
abline(v=quantile(samples,.8),col='blue')

#3e6 which values of p contain the narrowest interval equal to 66% of the posterior probability?

HPDI(samples, prob=.66)

#       |0.66     0.66| 
#       0.5205205 0.7847848

#3e7 which values of p contain 66% of the posterior probability, asuming equal posterior probabilty both below and above the interval?

PI(samples, prob=.66)
#   17%       83% 
#  0.5005005 0.7687688 





#3m1
p_grid<-seq(from=0,to=1,length.out=1000)
#p<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)  #this is basically a uniform prior (I think)
likelihood <- dbinom(8,size=15,prob=p_grid)
unstd.posterior <- likelihood * prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot (p_grid,posterior,type='b',xlab = 'probabilty of water',ylab = 'posterior prob')


#3m2
samples<-sample(p_grid,  prob = posterior,size = 1e4,replace = T)
HPDI(samples, prob=.9)
#     |0.9      0.9| 
#     0.3383383 0.7317317 

#3m3  Construct posertior predictie check for hti smodel.  simulate the distributino of samples, averagoe over the posterior uncertain in p.  What is prob of obvserving 8 water in 15 tosses?




dummy_w<-rbinom(1e5,size = 15, prob = samples)  
sum(dummy_w[dummy_w==8])/sum(dummy_w)  #[1] 0.1494651
table(dummy_w)/1e5
simplehist(dummy_w)


#3m4  Using the above data, calculate the prob of abserving 6 in 9 tosses.
samples<-sample(p_grid,  prob = posterior,size = 1e4,replace = T)

dummy_w<-rbinom(1e4,size = 9, prob = samples)  
table(dummy_w)/1e4
simplehist(dummy_w)

sum(dummy_w[dummy_w==6])/sum(dummy_w)  #[1] 0.2263661




#3m5

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)
#3h1
#3h2
#3h3
#3h4
#3h5





