
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

